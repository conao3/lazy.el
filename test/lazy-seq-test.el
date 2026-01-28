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
