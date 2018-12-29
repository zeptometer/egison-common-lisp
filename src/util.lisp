(defpackage :egison.util
  (:use :common-lisp)
  (:export mockable-gensym))

(in-package :egison.util)

;; CL:GENSYM is not mockable because we cannot change variables in CL package.
;; This is just a proxy function so that we can mock it.
(defun mockable-gensym () (gensym))
