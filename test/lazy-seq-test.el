;;; -*- lexical-binding: t; -*-

;; $ cd /path/to/lazy.el
;; $ emacs -L . --batch -l ert -l test/lazy-seq-test.el -f ert-run-tests-batch-and-exit

(require 'lazy)
(require 'lazy-seq)

(setq max-lisp-eval-depth 10000
      max-specpdl-size 10000)

(ert-deftest lazy-seq-test-range ()
  (should (equal (lazy-into-list (lazy-take 5 (lazy-seq-range)))
                 '(0 1 2 3 4)))

  (should (equal (lazy-into-list (lazy-seq-range 5 10))
                 '(5 6 7 8 9)))

  (should (equal (lazy-into-list (lazy-seq-range 0 10 2))
                 '(0 2 4 6 8)))

  (should (null (lazy-into-list (lazy-seq-range 10 5)))))

(ert-deftest lazy-seq-test-primes ()
  (should (equal (lazy-into-list (lazy-take 10 (lazy-seq-primes)))
                 '(2 3 5 7 11 13 17 19 23 29))))

(ert-deftest lazy-seq-test-fibonacci ()
  (should (= (lazy-elt 10 (lazy-seq-fibonacci))
             55))

  (should (= (lazy-elt 89 (lazy-seq-fibonacci))
             1779979416004714189)))

(ert-deftest lazy-seq-test-cl-loop ()
  (should (equal (cl-loop repeat 10 for i lazy-by (lazy-seq-primes) collect i)
                 '(2 3 5 7 11 13 17 19 23 29))))

(ert-deftest lazy-seq-test-from-list ()
  (should (equal (lazy-into-list (lazy-seq-from-list '(1 2 3 4 5)))
                 '(1 2 3 4 5)))
  (should (equal (lazy-into-list (lazy-seq-from-list nil))
                 nil)))

(ert-deftest lazy-seq-test-unfold ()
  (should (equal (lazy-into-list (lazy-take 5 (lazy-seq-unfold
                                         (lambda (n)
                                           (when (< n 10)
                                             (cons n (1+ n))))
                                         0)))
                 '(0 1 2 3 4))))

(ert-deftest lazy-seq-test-random ()
  (let ((randoms (lazy-into-list (lazy-take 10 (lazy-seq-random 100)))))
    (should (= (length randoms) 10))
    (should (cl-every (lambda (x) (and (>= x 0) (< x 100))) randoms)))
  (let ((floats (lazy-into-list (lazy-take 10 (lazy-seq-random)))))
    (should (= (length floats) 10))
    (should (cl-every (lambda (x) (and (>= x 0.0) (< x 1.0))) floats))))

(ert-deftest lazy-seq-test-powers ()
  (should (equal (lazy-into-list (lazy-take 5 (lazy-seq-powers 2)))
                 '(1 2 4 8 16)))
  (should (equal (lazy-into-list (lazy-take 5 (lazy-seq-powers 3)))
                 '(1 3 9 27 81)))
  (should (equal (lazy-into-list (lazy-take 4 (lazy-seq-powers 2 3)))
                 '(8 16 32 64))))

(ert-deftest lazy-seq-test-repeat ()
  (should (equal (lazy-into-list (lazy-seq-repeat 3 1 2 3))
                 '(1 2 3 1 2 3 1 2 3)))
  (should (equal (lazy-into-list (lazy-take 10 (lazy-seq-repeat nil 1 2)))
                 '(1 2 1 2 1 2 1 2 1 2))))

(ert-deftest lazy-seq-test-take-while ()
  (should (equal (lazy-into-list (lazy-seq-take-while (lambda (x) (< x 5))
                                                 '(1 2 3 4 5 6 7)))
                 '(1 2 3 4)))
  (should (equal (lazy-into-list (lazy-seq-take-while (lambda (x) (/= x ?d))
                                                 "abcdefg"))
                 '(?a ?b ?c)))
  (should (equal (lazy-into-list (lazy-seq-take-while (lambda (x) (< x 5))
                                                 [1 2 3 4 5 6]))
                 '(1 2 3 4))))
