(declaim (optimize (debug 3) (safety 3)))

(defpackage :egison
  (:use :common-lisp
        :optima)
  (:export match-all
           match-first
           SomethingM
           EqM
           IntegerM
           ListM
           MultisetM))

(in-package :egison)

(eval-when (:compile-toplevel)
  (defun compile-clause-all (value matcher clause)
    (destructuring-bind (pattern &body body) clause
      `(loop :for ret :in (gen-match-results ,pattern ,matcher ,value)
          :collect (destructuring-bind ,(extract-pattern-variables pattern) ret
                     ,@body))))

  (defun compile-clause-first (value matcher clause label)
    (destructuring-bind (pattern &body body) clause
      (let ((tmp (gensym)))
        `(let ((,tmp (gen-match-results ,pattern ,matcher ,value)))
           (when ,tmp
             (destructuring-bind ,(extract-pattern-variables pattern) ,tmp
               (return-from ,label (progn ,@body))))))))

  (defun extract-pattern-variables (pattern)
    (match pattern
      ((cons 'val _) nil)
      ((cons _ args) (mapcan #'extract-pattern-variables args))
      ('_ nil)
      ((guard x (symbolp x)) (list x))
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
    ((mstate- :mstack (cons (list (cons 'val f) matcher value) mstack)
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
    ((mstate- :mstack (cons (list (guard pvar (symbolp pvar)) :Something value) mstack)
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

(defparameter EqM
  #'(lambda (pattern value)
      (match pattern
        ((list 'val x) (if (eql x value) (list nil) nil))
        ((guard pvar (symbolp pvar)) (list (list (list pvar SomethingM value))))
        (_ (error "invalid pattern")))))

(defparameter IntegerM EqM)

(defun unjoin-l (list)
  (append (loop :for x :on list :collect x) '(nil)))

(defun unjoin-r (l)
  (cons nil
        (loop :for x :in l
           :for xs := (list x) :then (append xs (list x))
           :collect xs)))

(defun ListM (matcher)
  #'(lambda (pattern value)
      (match pattern
        ((list 'cons pattern-l pattern-r)
         (match value
           ((cons value-l value-r) `(((,pattern-l ,matcher ,value-l)
                                      (,pattern-r ,matcher ,value-r))))
           (_ nil)))
        ((list 'join pattern-l pattern-r)
         (mapcar #'(lambda (value-l value-r) `((,pattern-l ,(ListM matcher) ,value-l)
                                               (,pattern-r ,(ListM matcher) ,value-r)))
                 (unjoin-l value)
                 (unjoin-r value)))
        ((list 'val x) (if (eql x value) (list nil) nil))
        ((guard pvar (symbolp pvar)) `(((,pvar ,SomethingM ,value)))))))

(defun MultisetM (matcher)
  #'(lambda (pattern value)
      (match pattern
        ((list 'cons pattern-l pattern-r)
         (mapcar #'(lambda (cell) `((,pattern-l ,matcher ,(car cell))
                                    (,pattern-r ,(MultisetM matcher) ,(cdr cell))))
                 (match-all value (ListM matcher) ('(join hs (cons x ts)) (cons x (append hs ts)))))         )
        ((guard pvar (symbolp pvar))
         `(((,pvar ,SomethingM ,value)))))))
