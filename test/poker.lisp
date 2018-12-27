(defpackage :egison.poker.test
  (:use :common-lisp
        :egison
        :egison.poker
        :fiveam)
  (:export :egison-poker-test))

(in-package :egison.poker.test)

(def-suite egison-poker-test)
(in-suite egison-poker-test)

(test ModM
  (is (equal '(7) (match-all 20 (ModM 13) ('x x))))
  (is (equal '(:success) (match-all 15 (ModM 7) (`(val ,(lambda () 8)) :success))))
  (is (equal '(:success) (match-all 15 (ModM 7) (`(val ,(lambda () 22)) :success)))))

(test CardM
  (is (equal '((:club 12))
             (match-all '(card :club 12) (CardM)
               (`(card suit num) (list suit num)))))
  (is (equal '(:success)
             (match-all '(card :club 12) (CardM)
               (`(card (val ,(lambda () :club)) (val ,(lambda () 12))) :success))))
  (is (equal '(:success)
             (match-all '((card :spade 11) (card :spade 12)) (ListM (CardM))
               (`(cons (card suit num)
                       (cons (card (val ,(lambda (suit num) suit)) (val ,(lambda (suit num) (+ 1 num))))
                             _))
                 :success))))
  (is (equal '(:success)
             (match-all '((card :spade 12) (card :spade 11)) (MultisetM (CardM))
               (`(cons (card suit num)
                       (cons (card (val ,(lambda (suit num) suit)) (val ,(lambda (suit num) (+ 1 num))))
                             _))
                 :success)))))

(test poker-hand
  (is (string= "Straight flush"
              (poker-hand `((card :club 12)
                            (card :club 10)
                            (card :club 13)
                            (card :club 1)
                            (card :club 11)))))

  (is (string= "Full house"
              (poker-hand `((card :diamond 1)
                            (card :club 2)
                            (card :club 1)
                            (card :heart 1)
                            (card :diamond 2)))))

  (is (string= "Straight"
              (poker-hand `((card :diamond 4)
                            (card :club 2)
                            (card :club 5)
                            (card :heart 1)
                            (card :diamond 3)))))

  (is (string= "Nothing"
              (poker-hand `((card :diamond 4)
                            (card :club 10)
                            (card :club 5)
                            (card :heart 1)
                            (card :diamond 3))))))
