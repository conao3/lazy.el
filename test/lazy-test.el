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

(ert-deftest lazy-test-concat ()
  (should (equal (lazy-into-list (lazy-take (lazy-concat (lazy-cons (lazy-seq-range 0 3)
                                                                (lazy-cons (lazy-seq-range 10 13)
                                                                          (lazy-nil))))
                                        6))
                 '(0 1 2 10 11 12))))

(ert-deftest lazy-test-interleave ()
  (should (equal (lazy-into-list (lazy-take (lazy-interleave (lazy-seq-range 0 3)
                                                        (lazy-seq-range 10 13))
                                        6))
                 '(0 10 1 11 2 12))))

(ert-deftest lazy-test-cycle ()
  (should (equal (lazy-into-list (lazy-take (lazy-cycle (lazy-seq-range 0 3)) 10))
                 '(0 1 2 0 1 2 0 1 2 0))))

(ert-deftest lazy-test-repeat ()
  (should (equal (lazy-into-list (lazy-take (lazy-repeat 42) 5))
                 '(42 42 42 42 42))))

(ert-deftest lazy-test-repeatedly ()
  (let ((counter 0))
    (should (equal (lazy-into-list (lazy-take (lazy-repeatedly (lambda () (setq counter (1+ counter)))) 5))
                   '(1 2 3 4 5)))))

(ert-deftest lazy-test-iterate ()
  (should (equal (lazy-into-list (lazy-take (lazy-iterate (lambda (x) (* x 2)) 1) 5))
                 '(1 2 4 8 16))))

(ert-deftest lazy-test-distinct ()
  (should (equal (lazy-into-list (lazy-distinct (lazy-cons 1 (lazy-cons 2 (lazy-cons 1 (lazy-cons 3 (lazy-cons 2 (lazy-nil))))))))
                 '(1 2 3))))

(ert-deftest lazy-test-dedupe ()
  (should (equal (lazy-into-list (lazy-dedupe (lazy-cons 1 (lazy-cons 1 (lazy-cons 2 (lazy-cons 2 (lazy-cons 3 (lazy-nil))))))))
                 '(1 2 3))))

(ert-deftest lazy-test-reductions ()
  (should (equal (lazy-into-list (lazy-reductions #'+ (lazy-seq-range 1 5) 0))
                 '(0 1 3 6 10))))

(ert-deftest lazy-test-split-at ()
  (let ((result (lazy-split-at (lazy-seq-range 0 10) 5)))
    (should (equal (lazy-into-list (car result))
                   '(0 1 2 3 4)))
    (should (equal (lazy-into-list (cdr result))
                   '(5 6 7 8 9)))))

(ert-deftest lazy-test-split-with ()
  (let ((result (lazy-split-with (lambda (x) (< x 5)) (lazy-seq-range 0 10))))
    (should (equal (lazy-into-list (car result))
                   '(0 1 2 3 4)))
    (should (equal (lazy-into-list (cdr result))
                   '(5 6 7 8 9)))))

(ert-deftest lazy-test-map-indexed ()
  (should (equal (lazy-into-list (lazy-take (lazy-map-indexed (lambda (i x) (list i x))
                                                         (lazy-seq-range 10 15))
                                        3))
                 '((0 10) (1 11) (2 12)))))

(ert-deftest lazy-test-take-nth ()
  (should (equal (lazy-into-list (lazy-take-nth (lazy-seq-range 0 10) 2))
                 '(0 2 4 6 8))))

(ert-deftest lazy-test-some ()
  (should (equal (lazy-some (lambda (x) (and (> x 5) x)) (lazy-seq-range 0 10))
                 6))
  (should (null (lazy-some (lambda (x) (> x 100)) (lazy-seq-range 0 10)))))

(ert-deftest lazy-test-every ()
  (should (lazy-every (lambda (x) (>= x 0)) (lazy-seq-range 0 10)))
  (should (null (lazy-every (lambda (x) (< x 5)) (lazy-seq-range 0 10)))))

(ert-deftest lazy-test-keep ()
  (should (equal (lazy-into-list (lazy-keep (lambda (x) (and (cl-evenp x) x))
                                      (lazy-seq-range 0 10)))
                 '(0 2 4 6 8))))

(ert-deftest lazy-test-partition ()
  (should (equal (lazy-into-list (lazy-partition (lazy-seq-range 0 10) 3))
                 '((0 1 2) (3 4 5) (6 7 8))))
  (should (equal (lazy-into-list (lazy-partition (lazy-seq-range 0 5) 2))
                 '((0 1) (2 3)))))

(ert-deftest lazy-test-partition-by ()
  (should (equal (lazy-into-list (lazy-partition-by (lambda (x) (< x 5))
                                              (lazy-seq-range 0 10)))
                 '((0 1 2 3 4) (5 6 7 8 9))))
  (should (equal (lazy-into-list (lazy-partition-by (lambda (x) (cl-evenp x))
                                              (lazy-cons 0 (lazy-cons 2 (lazy-cons 1 (lazy-cons 3 (lazy-cons 4 (lazy-nil))))))))
                 '((0 2) (1 3) (4)))))

(ert-deftest lazy-test-flatten ()
  (should (equal (lazy-into-list (lazy-flatten (lazy-cons (lazy-seq-range 0 3)
                                                     (lazy-cons (lazy-seq-range 3 6)
                                                               (lazy-nil)))))
                 '(0 1 2 3 4 5)))
  (should (equal (lazy-into-list (lazy-flatten (lazy-cons 1
                                                     (lazy-cons (lazy-cons 2 (lazy-cons 3 (lazy-nil)))
                                                               (lazy-cons 4 (lazy-nil))))))
                 '(1 2 3 4))))
