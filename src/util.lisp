(defpackage :egison.util
  (:use :common-lisp)
  (:export mockable-gensym
           delay
           force
           lnil
           lcons
           lcar
           lcdr
           lappend
           ltake
           lmap
           filter))

(in-package :egison.util)

;; CL:GENSYM is not mockable because we cannot change variables in CL package.
;; This is just a proxy function so that we can mock it.
(defun mockable-gensym () (gensym))


;;; Lazy list
(defmacro delay (&body body)
  `#'(lambda () ,@body))

(defun force (thunk)
  (funcall thunk))

(defun lnil () (delay (values nil nil)))

(defmacro lcons (head tail)
  `(delay (values ,head ,tail)))

(defun lappend (lcell1 lcell2)
  (delay (multiple-value-bind (head tail) (force lcell1)
           (if (null tail)
               (force lcell2)
               (values head (lappend tail lcell2))))))

(defun lcar (lcell)
  (multiple-value-bind (a d) (force lcell)
    (declare (ignore d))
    a))

(defun lcdr (lcell)
  (multiple-value-bind (a d) (force lcell)
    (declare (ignore a))
    d))

(defun ltake (n lcell &optional (results nil))
  (if (zerop n)
      (reverse results)
      (multiple-value-bind (head tail) (force lcell)
        (if (null tail)
            (reverse results)
            (take (1- n) tail (cons head results))))))

(defun lmap (func lcell)
  (delay (multiple-value-bind (head tail) (force lcell)
           (if tail
               (values (funcall func head) (lmap func tail))
               (lnil)))))

(defun filter (pred lcell)
  (multiple-value-bind (head tail) (force lcell)
    (cond ((null tail) (lnil))
          ((funcall pred head) (lcons head (filter pred tail)))
          (t (filter pred tail)))))
