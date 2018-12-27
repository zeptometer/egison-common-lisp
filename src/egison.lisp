(declaim (optimize (debug 3) (safety 3)))

(defpackage :egison
  (:use :common-lisp
        :optima)
  (:export pattern-variable-p
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
           gen-match-results
           extract-pattern-variables))

(in-package :egison)

(defun pattern-variable-p (x)
  (and (symbolp x)
       (not (null x))))

(eval-when (:compile-toplevel)
  (defun compile-clause-all (value matcher clause)
    (destructuring-bind (pattern &body body) clause
      `(loop :for binds :in (gen-match-results ,pattern ,matcher ,value)
          :collect (destructuring-bind ,(extract-pattern-variables pattern) binds
                     ,@body))))

  (defun compile-clause-first (value matcher clause label)
    (destructuring-bind (pattern &body body) clause
      (let ((binds-sym (gensym)))
        `(let ((,binds-sym (gen-match-results ,pattern ,matcher ,value)))
           (when ,binds-sym
             (destructuring-bind ,(extract-pattern-variables pattern) (car ,binds-sym)
               (return-from ,label (progn ,@body))))))))

  (defun extract-pattern-variables (pattern)
    (match pattern
      ((cons 'val _) nil)
      ((cons _ args) (mapcan #'extract-pattern-variables args))
      ('_ nil)
      (nil nil)
      ((guard x (pattern-variable-p x)) (list x))
      (_ (error "invalid pattern")))))

(defmacro match-all (value matcher &body clauses)
  `(append ,@(mapcar #'(lambda (clause) (compile-clause-all value matcher clause)) clauses)))

(defmacro match-first (value matcher &body clauses)
  (let ((label (gensym)))
    `(block ,label
       ,@(mapcar #'(lambda (clause) (compile-clause-first value matcher clause label)) clauses))))

(defstruct mstate mstack bind)

(defun gen-match-results (pattern matcher value)
  (process-mstates (list (make-mstate :mstack (list (list pattern matcher value))
                                      :bind nil))
                   nil nil))

(defun process-mstates (mstates stack results)
  (cond ((and (null mstates) (null stack))
         results)
        ((null mstates)
         (process-mstates (car stack) (cdr stack) results))
        (t (match mstates
             ((cons (mstate- :mstack nil :bind bind) rest)
              (process-mstates rest stack (cons bind results)))
             ((cons mstate rest)
              (process-mstates (process-mstate mstate)
                               (cons rest stack)
                               results))))))

(defun process-mstate (mstate)
  (match mstate
    ;; val pattern
    ((mstate- :mstack (cons (list (list 'val f) matcher value) mstack)
              :bind bind)
     (let ((next-matomss (funcall matcher `(val ,(apply f bind)) value)))
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
    ;; other patterns
    ((mstate- :mstack (cons (list pattern matcher value) mstack)
              :bind bind)
     (let ((next-matomss (funcall matcher pattern value)))
       (mapcar #'(lambda (next-matoms)
                   (make-mstate :mstack (append next-matoms mstack)
                                :bind bind))
               next-matomss)))))

(defparameter SomethingM :Something)

(defun EqM (eq)
  #'(lambda (pattern value)
      (match pattern
        ((list 'val x) (if (funcall eq x value) (list nil) nil))
        ((guard pvar (pattern-variable-p pvar)) (list (list (list pvar SomethingM value))))
        (_ (error "invalid pattern")))))

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
        ((guard pvar (pattern-variable-p pvar)) `(((,pvar ,SomethingM ,value)))))))

(defun MultisetM (matcher)
  #'(lambda (pattern value)
      (match pattern
        (nil (if (null value) (list nil) nil))
        ((list 'cons pattern-l pattern-r)
         (mapcar #'(lambda (cell) `((,pattern-l ,matcher ,(car cell))
                                    (,pattern-r ,(MultisetM matcher) ,(cdr cell))))
                 (match-all value (ListM matcher) ('(join hs (cons x ts)) (cons x (append hs ts)))))         )
        ((list 'val x) (if (equal x value) (list nil) nil))
        ((guard pvar (pattern-variable-p pvar))
         `(((,pvar ,SomethingM ,value)))))))


