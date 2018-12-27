(defpackage :egison.test
  (:use :common-lisp
        :egison
        :fiveam)
  (:export :run!
           :egison-test))

(in-package :egison.test)

(def-suite egison-test)
(in-suite egison-test)

(test extract-pattern-variables
  (is (equal (extract-pattern-variables ''x) '(x)))
  (is (equal (extract-pattern-variables '`y) '(y)))
  (is (equal (extract-pattern-variables ''_) nil))
  (is (equal (extract-pattern-variables 'nil) nil))
  (is (equal (extract-pattern-variables '`(val ,(lambda (x) (+ x 1)))) nil))
  (is (equal (extract-pattern-variables '`(join x (cons (join y (cons _ z)) _))) '(x y z))))

(test unjoin
  (is (equal (unjoin-l nil) '(nil)))
  (is (equal (unjoin-l '(1 2 3)) '(nil (1) (1 2) (1 2 3))))
  (is (equal (unjoin-r nil) '(nil)))
  (is (equal (unjoin-r '(1 2 3)) '((1 2 3) (2 3) (3) nil))))

(test SomethingM
  (is (equal (match-all 1 SomethingM ('_ 10)) '(10)))
  (is (equal (match-all 1 SomethingM ('x x)) '(1))))

(test EqM/IntegerM
  (is (equal (match-all 1 IntegerM ('_ 10)) '(10)))
  (is (equal (match-all 1 IntegerM ('x x)) '(1)))
  (is (equal (match-all 1 IntegerM (`(val ,(lambda () 1)) 1)) '(1)))
  (is (equal (match-all 10 IntegerM (`(val ,(lambda () 1)))) '())))

(test ListM
  (is (equal (match-all '(1 2) (ListM IntegerM)
               (`(cons x y) (list x y)))
             '((1 (2)))))
  (is (equal (match-all '(1 2) (ListM IntegerM)
               (`(cons (val ,(lambda () 1)) _) 1))
             '(1)))
  (is (equal (match-all '(1 2) (ListM IntegerM)
               (`(join xs ys) (list xs ys)))
             '(((1 2) nil) ((1) (2)) (nil (1 2)))))
  (is (equal (match-all '(1 2 3 4 5) (ListM IntegerM)
               ('(cons _ (cons _ (cons _ (cons _ (cons x _))))) x))
             '(5))))

(test MultisetM
  (is (equal (match-all '(1 2 3) (MultisetM IntegerM)
               (`(cons x _) x))
             '(1 2 3)))
  (is (equal (match-all '(1 2 3) (MultisetM IntegerM)
               (`(cons _ x) x))
             '((2 3) (1 3) (1 2)))))

(test pattern-variable
  (is (equal (match-all '(1 2) (ListM IntegerM)
               (`(cons x (cons (val ,(lambda (x) (+ 1 x))) nil)) :success))
             '(:success)))
  (is (equal (match-all '(1 2 3) (ListM IntegerM)
               (`(cons x (cons y (cons (val ,(lambda (x y) (+ x y))) nil))) :success))
             '(:success))))

(test multiclause
  (is (equal (match-all 1 IntegerM
               ('x x)
               ('_ 20))
             '(1 20))))
