(defpackage :egison
  (:use :common-lisp
        :egison.util
        :optima)
  (:export pattern-variable-p
           match-lazy
           match-all
           match-first
           SomethingM
           EqM
           IntegerM
           ListM
           MultisetM
           _
           val
           cons
           join
           ;; visible for testing
           unjoin-l
           unjoin-r
           compile-pattern))

(in-package :egison)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun pattern-variable-p (x)
    (and (symbolp x)
         (not (eq x '_))
         (not (null x))
         (not (eq x t))))

  (defun compile-pattern-args (args vars compiled-patterns)
    (if (null args)
        (values (reverse compiled-patterns) vars)
        (multiple-value-bind (compiled post-vars) (compile-pattern (car args) vars)
          (compile-pattern-args (cdr args) post-vars (cons compiled compiled-patterns)))))

  (defun compile-pattern (pattern vars)
    (match pattern
      (nil (values nil vars))
      ('_ (values ''_ vars))
      ((guard x (pattern-variable-p x)) (values `',x (append vars (list x))))
      ((cons 'val body) (let ((tmp (mockable-gensym)))
                          (values
                           `(list 'val #'(lambda (,tmp) (destructuring-bind ,vars ,tmp
                                                          (declare (ignorable ,@vars))
                                                          ,@body)))
                           vars)))
      ((cons destructor args) (multiple-value-bind (compiled-patterns post-vars) (compile-pattern-args args vars nil)
                                  (values `(list ',destructor ,@compiled-patterns) post-vars)) )
      (_ (error "invalid pattern"))))

  (defun compile-clause (value matcher clause)
    (destructuring-bind (pattern &body body) clause
      (multiple-value-bind (compiled-pattern vars) (compile-pattern pattern nil)
        (let ((tmp (gensym)))
          `(lmap #'(lambda (,tmp)
                     (destructuring-bind ,vars ,tmp
                       (declare (ignorable ,@vars))
                       ,@body))
                 (gen-match-results ,compiled-pattern ,matcher ,value))))))

  (defun compile-clauses (value matcher clauses)
    (if (null clauses)
        `(lnil)
        `(lappend ,(compile-clause value matcher (car clauses))
                  ,(compile-clauses value matcher (cdr clauses))))))

(defmacro match-lazy (value matcher &body clauses)
  (compile-clauses value matcher clauses))

(defmacro match-all (value matcher &body clauses)
  `(ltake -1 (match-lazy ,value ,matcher ,@clauses)))

(defmacro match-first (value matcher &body clauses)
  `(lcar (match-lazy ,value ,matcher ,@clauses)))

(defstruct mstate mstack bind)

(defun gen-match-results (pattern matcher value)
  (process-mstates (list (make-mstate :mstack (list (list pattern matcher value))
                                      :bind nil))
                   nil))

(defun process-mstates (mstates stack)
  (cond ((and (null mstates) (null stack))
         (lnil))
        ((null mstates)
         (process-mstates (car stack) (cdr stack)))
        (t (match mstates
             ((cons (mstate- :mstack nil :bind bind) rest)
              (lcons bind (process-mstates rest stack)))
             ((cons mstate rest)
              (process-mstates (process-mstate mstate)
                               (cons rest stack)))))))

(defun process-mstate (mstate)
  (match mstate
    ;; val pattern
    ((mstate- :mstack (cons (list (list 'val f) matcher value) mstack)
              :bind bind)
     (let ((next-matomss (funcall matcher `(val ,(funcall f bind)) value)))
       (mapcar #'(lambda (next-matoms)
                   (make-mstate :mstack (append next-matoms mstack)
                                :bind bind))
               next-matomss)))
    ;; _ pattern
    ((mstate- :mstack (cons (list '_ :Something _) mstack)
              :bind bind)
     (list (make-mstate :mstack mstack :bind bind)))
    ;; pattern variable
    ((mstate- :mstack (cons (list (guard pvar (pattern-variable-p pvar)) :Something value) mstack)
              :bind bind)
     (list (make-mstate :mstack mstack :bind (append bind (list value)))))
    ;; and pattern
    ((mstate- :mstack (cons (list (list 'and pattern-l pattern-r) matcher value) mstack)
              :bind bind)
     (list (make-mstate :mstack (cons (list pattern-l matcher value) (cons (list pattern-r matcher value) mstack)) :bind bind)))
    ;; or pattern
    ((mstate- :mstack (cons (list (list 'or pattern-l pattern-r) matcher value) mstack)
              :bind bind)
     (list (make-mstate :mstack (cons (list pattern-l matcher value) mstack) :bind bind)
           (make-mstate :mstack (cons (list pattern-r matcher value) mstack) :bind bind)))
    ;; other patterns
    ((mstate- :mstack (cons (list pattern matcher value) mstack)
              :bind bind)
     (let ((next-matomss (funcall matcher pattern value)))
       (mapcar #'(lambda (next-matoms)
                   (make-mstate :mstack (append next-matoms mstack)
                                :bind bind))
               next-matomss)))))

(defun SomethingM () :Something)

(defun EqM (eq)
  #'(lambda (pattern value)
      (match pattern
        ((list 'val x) (if (funcall eq x value) (list nil) nil))
        (_ `(((,pattern ,(SomethingM) ,value)))))))

(defun IntegerM () (EqM #'eql))

(defun unjoin-r (list)
  (append (loop :for x :on list :collect x) '(nil)))

(defun unjoin-l (l)
  (cons nil
        (loop :for x :in l
           :for xs := (list x) :then (append xs (list x))
           :collect xs)))

(defun ListM (matcher)
  #'(lambda (pattern value)
      (match pattern
        (nil (if (null value) '(nil) nil))
        ((list 'cons pattern-l pattern-r)
         (match value
           ((cons value-l value-r) `(((,pattern-l ,matcher ,value-l)
                                      (,pattern-r ,(ListM matcher) ,value-r))))
           (_ nil)))
        ((list 'join pattern-l pattern-r)
         (mapcar #'(lambda (value-l value-r) `((,pattern-l ,(ListM matcher) ,value-l)
                                               (,pattern-r ,(ListM matcher) ,value-r)))
                 (unjoin-l value)
                 (unjoin-r value)))
        ((list 'val x) (if (equal x value) (list nil) nil))
        (_ `(((,pattern ,(SomethingM) ,value)))))))

(defun MultisetM (matcher)
  #'(lambda (pattern value)
      (match pattern
        (nil (if (null value) (list nil) nil))
        ((list 'cons pattern-l pattern-r)
         (mapcar #'(lambda (cell) `((,pattern-l ,matcher ,(car cell))
                                    (,pattern-r ,(MultisetM matcher) ,(cdr cell))))
                 (match-all value (ListM matcher) ((join hs (cons x ts)) (cons x (append hs ts))))))
        ((list 'val x) (if (equal x value) (list nil) nil))
        (_ `(((,pattern ,(SomethingM) ,value)))))))

;;; Reader Macro
(set-macro-character #\$ #'(lambda (stream char) (declare (ignore char))
                             (list 'val (read stream))))
