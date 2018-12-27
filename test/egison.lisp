(defpackage :egison.test
  (:use :common-lisp
        :egison
        :fiveam)
  (:export :egison-test))

(in-package :egison.test)

(def-suite egison-test)
(in-suite egison-test)

(test extract-pattern-variables
  (is (equal '(x) (extract-pattern-variables ''x)))
  (is (equal '(y) (extract-pattern-variables '`y)))
  (is (equal nil (extract-pattern-variables ''_)))
  (is (equal nil (extract-pattern-variables 'nil)))
  (is (equal nil (extract-pattern-variables '`(val ,(lambda (x) (+ x 1))))))
  (is (equal '(x y z) (extract-pattern-variables '`(join x (cons (join y (cons _ z)) _))))))

(test unjoin
  (is (equal '(nil) (unjoin-l nil)))
  (is (equal '(nil (1) (1 2) (1 2 3)) (unjoin-l '(1 2 3))))
  (is (equal '(nil) (unjoin-r nil)))
  (is (equal '((1 2 3) (2 3) (3) nil) (unjoin-r '(1 2 3)))))

(test SomethingM
  (is (equal '(10) (match-all 1 SomethingM ('_ 10))))
  (is (equal '(1) (match-all 1 SomethingM ('x x)))))

(test EqM/IntegerM
  (is (equal '(10) (match-all 1 (IntegerM) ('_ 10))))
  (is (equal '(1) (match-all 1 (IntegerM) ('x x))))
  (is (equal '(1) (match-all 1 (IntegerM) (`(val ,(lambda () 1)) 1))))
  (is (equal '() (match-all 10 (IntegerM) (`(val ,(lambda () 1)))))))

(test ListM
  (is (equal '((1 (2)))
             (match-all '(1 2) (ListM (IntegerM))
               (`(cons x y) (list x y)))))
  (is (equal '(1)
             (match-all '(1 2) (ListM (IntegerM))
               (`(cons (val ,(lambda () 1)) _) 1))))
  (is (equal '(((1 2) nil) ((1) (2)) (nil (1 2)))
             (match-all '(1 2) (ListM (IntegerM))
               (`(join xs ys) (list xs ys)))))
  (is (equal '(5)
             (match-all '(1 2 3 4 5) (ListM (IntegerM))
               ('(cons _ (cons _ (cons _ (cons _ (cons x _))))) x))))
  (is (equal '(:success)
             (match-all '(1 2) (ListM (IntegerM))
               (`(val ,(lambda () '(1 2))) :success)))))

(test MultisetM
  (is (equal '(1 2 3)
             (match-all '(1 2 3) (MultisetM (IntegerM))
               (`(cons x _) x))))
  (is (equal '((2 3) (1 3) (1 2))
             (match-all '(1 2 3) (MultisetM (IntegerM))
               (`(cons _ x) x))))
  (is (equal '((1 2) (1 3) (2 1) (2 3) (3 1) (3 2))
             (match-all '(1 2 3) (MultisetM (IntegerM))
               (`(cons x (cons y _)) (list x y)))))
  (is (equal nil
             (match-all '(1 2 3) (MultisetM (IntegerM))
               ('nil :success))))
  (is (equal '(:success)
             (match-all '(1 2) (MultisetM (IntegerM))
               (`(cons (val ,(lambda () 1))
                  (cons (val ,(lambda () 2)) nil)) :success)))))

(test pattern-variable
  (is (equal '(:success)
             (match-all '(1 2) (ListM (IntegerM))
               (`(cons x (cons (val ,(lambda (x) (+ 1 x))) nil)) :success))))
  (is (equal '(:success)
             (match-all '(1 2 3) (ListM (IntegerM))
               (`(cons x (cons y (cons (val ,(lambda (x y) (+ x y))) nil))) :success)))))

(test multiclause
  (is (equal '(1 20)
             (match-all 1 (IntegerM)
               ('x x)
               ('_ 20)))))

(test match-first
  (is (equal 2
             (match-first '(1 2) (MultisetM (IntegerM))
               (`(cons x (val ,(lambda (x) (list 1)))) x)))))
