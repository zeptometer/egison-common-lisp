(defpackage :egison.util.test
  (:use :common-lisp
        :fiveam
        :mockingbird
        :egison.util)
  (:export :egison-util-test))

(in-package :egison.util.test)

(def-suite egison-util-test)
(in-suite egison-util-test)

(defun nat (&optional (n 0))
  (lcons n (nat (+ n 1))))

(defun fib (&optional (a 1) (b 1))
  (lcons a (fib b (+ a b))))

(defun primes (&optional (nats (nat 2)))
  (multiple-value-bind (head tail) (force nats)
    (lcons head (primes (filter #'(lambda (x) (not (zerop (rem x head)))) tail)))))

(test infinite-lazy-list
  (is (equal '(0 1 2 3 4) (ltake 5 (nat))))
  (is (equal '(1 1 2 3 5) (ltake 5 (fib))))
  (is (equal '(2 3 5 7 11) (ltake 5 (primes)))))
