;;; -*- lexical-binding: t; -*-

;; $ cd /path/to/lazy.el
;; $ emacs -L . --batch -l ert -l test/lazy-test.el -f ert-run-tests-batch-and-exit

(require 'lazy)
(require 'lazy-seq)

(setq max-lisp-eval-depth 10000
      max-specpdl-size 10000)

(ert-deftest lazy-test-error-check ()
  (should (null (lazy-into-list (lazy-seq-range 10 5))))

  (should (null (lazy-into-list (lazy-take (lazy-seq-range) -1))))

  (should (null (lazy-into-list (lazy-take (lazy-drop (lazy-seq-range) -1) 0))))

  (should (lazy-stream-p (lazy-seq-range)))

  (should (null (lazy-stream-p [1]))))

(ert-deftest lazy-test-append ()
  (should (equal (lazy-into-list (lazy-append (lazy-take (lazy-seq-range) 3) (lazy-take (lazy-seq-range) 3)))
                 '(0 1 2 0 1 2)))

  (should (equal (lazy-into-list (lazy-take (lazy-append (lazy-seq-range 0 3) (lazy-seq-range)) 6))
                 '(0 1 2 0 1 2)))

  (should (equal (lazy-into-list (lazy-append))
                 nil)))

(ert-deftest lazy-test-pop ()
  (should (equal (let ((r (lazy-seq-range))) (list (lazy-pop r) (lazy-pop r) (lazy-pop r)))
                 '(0 1 2))))

(ert-deftest lazy-test-subseq ()
  (should (equal (lazy-into-list (lazy-subseq (lazy-seq-range) 5 10))
                 '(5 6 7 8 9))))

(ert-deftest lazy-test-mapn ()
  (should (equal (lazy-into-list (lazy-take (lazy-mapn #'string
                                                 (lazy-seq-range ?A)
                                                 (lazy-seq-range ?a)
                                                 (lazy-seq-range ?0))
                                        10))
                 '("Aa0" "Bb1" "Cc2" "Dd3" "Ee4" "Ff5" "Gg6" "Hh7" "Ii8" "Jj9"))))
