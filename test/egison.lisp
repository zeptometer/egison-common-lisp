(defpackage :egison.test
  (:use :common-lisp
        :fiveam
        :mockingbird
        :egison
        :egison.util)
  (:export :egison-test))

(in-package :egison.test)

(def-suite egison-test)
(in-suite egison-test)

(test pattern-variable-p
  (is-true (pattern-variable-p 'x))
  (is-true (pattern-variable-p 'y))
  (is-false (pattern-variable-p '_))
  (is-false (pattern-variable-p nil))
  (is-false (pattern-variable-p t))
  (is-false (pattern-variable-p 1))
  (is-false (pattern-variable-p "hello")))

(defun test-compile-pattern (expected-compiled expected-post-vars pattern pre-vars)
  (multiple-value-bind (compiled post-vars) (compile-pattern pattern pre-vars)
    (is (equal expected-compiled compiled))
    (is (equal expected-post-vars post-vars))))

(test compile-pattern
  (test-compile-pattern nil '(x) 'nil '(x))
  (test-compile-pattern ''x '(y x) 'x '(y))
  (test-compile-pattern ''_ '(x) '_ '(x))
  (with-dynamic-stubs ((mockable-gensym 'neko))
    (test-compile-pattern
     '(list 'val #'(lambda (neko) (destructuring-bind (x y) neko (declare (ignorable x y)) (+ 1 x))))
     '(x y)
     '(val (+ 1 x))
     '(x y))
    (test-compile-pattern
     '(list 'cons 'x (list 'val #'(lambda (neko) (destructuring-bind (y x) neko (declare (ignorable y x)) (+ x y)))))
     '(y x)
     '(cons x (val (+ x y)))
     '(y))
    (test-compile-pattern
     '(list 'cons 'x (list 'cons 'y (list 'cons 'z '_)))
     '(x y z)
     '(cons x (cons y (cons z _)))     
     nil)))

(test unjoin
  (is (equal '(nil) (unjoin-l nil)))
  (is (equal '(nil (1) (1 2) (1 2 3)) (unjoin-l '(1 2 3))))
  (is (equal '(nil) (unjoin-r nil)))
  (is (equal '((1 2 3) (2 3) (3) nil) (unjoin-r '(1 2 3)))))

(test SomethingM
  (is (equal '(10) (match-all 1 (SomethingM) (_ 10))))
  (is (equal '(1) (match-all 1 (SomethingM) (x x)))))

(test EqM/IntegerM
  (is (equal '(10) (match-all 1 (IntegerM) (_ 10))))
  (is (equal '(1) (match-all 1 (IntegerM) (x x))))
  (is (equal '(1) (match-all 1 (IntegerM) ($1 1))))
  (is (equal '() (match-all 10 (IntegerM) ($1)))))

(test ListM
  (is (equal '((1 (2)))
             (match-all '(1 2) (ListM (IntegerM))
               ((cons x y) (list x y)))))
  (is (equal '(1)
             (match-all '(1 2) (ListM (IntegerM))
               ((cons $1 _) 1))))
  (is (equal '((nil (1 2)) ((1) (2)) ((1 2) nil))
             (match-all '(1 2) (ListM (IntegerM))
               ((join xs ys) (list xs ys)))))
  (is (equal '(5)
             (match-all '(1 2 3 4 5) (ListM (IntegerM))
               ((cons _ (cons _ (cons _ (cons _ (cons x _))))) x))))
  (is (equal '(:success)
             (match-all '(1 2) (ListM (IntegerM))
               ($'(1 2) :success)))))

(test MultisetM
  (is (equal '(1 2 3)
             (match-all '(1 2 3) (MultisetM (IntegerM))
               ((cons x _) x))))
  (is (equal '(1 2 3)
             (match-all '(1 2 3) (MultisetM (IntegerM))
               ((cons x _) x))))
  (is (equal '((2 3) (1 3) (1 2))
             (match-all '(1 2 3) (MultisetM (IntegerM))
               ((cons _ x) x))))
  (is (equal '((1 2) (1 3) (2 1) (2 3) (3 1) (3 2))
             (match-all '(1 2 3) (MultisetM (IntegerM))
               ((cons x (cons y _)) (list x y)))))
  (is (equal nil
             (match-all '(1 2 3) (MultisetM (IntegerM))
               (nil :success))))
  (is (equal '(:success)
             (match-all '(1 2) (MultisetM (IntegerM))
               ((cons $1 (cons $2 nil)) :success)))))

(test pattern-variable
  (is (equal '(:success)
             (match-all '(1 2) (ListM (IntegerM))
               ((cons x (cons $(+ 1 x) nil)) :success))))
  (is (equal '(:success)
             (match-all '(1 2 3) (ListM (IntegerM))
               ((cons x (cons y (cons $(+ x y) nil))) :success)))))

(test multiclause
  (is (equal '(1 20)
             (match-all 1 (IntegerM)
               (x x)
               (_ 20)))))

(test match-first
  (is (equal 2
             (match-first '(1 2) (MultisetM (IntegerM))
               ((cons x $'(1)) x)))))

(test and-pattern
  (is (equal '(:success)
             (match-all '(1 2 3) (MultisetM (IntegerM))
               ((and (cons $1 _) (cons $2 _)) :success))))
  (is (equal nil
             (match-all '(1 2 3) (MultisetM (IntegerM))
               ((and (cons $4 _) (cons $2 _)) :success)))))

(test or-pattern
  (is (equal '(:success)
             (match-all '(1 2 3) (MultisetM (IntegerM))
               ((or (cons $3 _) (cons $5 _)) :success))))
  (is (equal nil
             (match-all '(1 2 3) (MultisetM (IntegerM))
               ((or (cons $4 _) (cons $5 _)) :success)))))
