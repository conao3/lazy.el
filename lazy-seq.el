;;; lazy-seq.el --- Lazy sequence generators              -*- lexical-binding: t; -*-

;; Copyright (C) 2018  chuntaro
;; Copyright (C) 2026  conao3

;; Author: chuntaro <chuntaro@sakura-games.jp>
;; Maintainer: conao3 <conao3@gmail.com>
;; URL: https://github.com/chuntaro/lazy.el
;; Package-Requires: ((emacs "25") (lazy "0.1.0"))
;; Keywords: lisp, lazy
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Lazy sequence generator functions using lazy.el.

;;; Code:

(require 'lazy)

(defun lazy-seq-range (&optional start end step)
  "Create a lazy stream of numbers from START to END by STEP."
  (unless start (setq start 0))
  (and end (> start end) (setq end start))
  (unless step (setq step 1))
  (lazy
   (if (and end (= start end))
       (lazy-nil)
     (lazy-cons start (lazy-seq-range (+ start step) end step)))))

(defun lazy-seq--sieve (stream)
  "Sieve of Eratosthenes for STREAM."
  (lazy
   (lazy-cons (lazy-car stream)
              (lazy-seq--sieve (lazy-filter (lambda (x)
                                          (/= 0 (% x (lazy-car stream))))
                                        (lazy-cdr stream))))))

(defun lazy-seq-primes ()
  "Return an infinite lazy stream of prime numbers."
  (lazy-seq--sieve (lazy-seq-range 2)))

(defun lazy-seq-fibonacci ()
  "Return an infinite lazy stream of Fibonacci numbers."
  (lazy
   (cl-labels ((rec (a b)
                 (lazy-cons (+ a b)
                            (rec b (+ a b)))))
     (lazy-cons 0 (lazy-cons 1 (rec 0 1))))))

(defun lazy-seq-naturals (&optional start)
  "Return an infinite lazy stream of natural numbers starting from START.
If START is not provided, starts from 1."
  (lazy-seq-range (or start 1)))

(defun lazy-seq-from-list (list)
  "Convert LIST to a lazy stream."
  (if (null list)
      (lazy-nil)
    (lazy-cons (car list)
               (lazy-seq-from-list (cdr list)))))

(defun lazy-seq-unfold (function seed)
  "Return an infinite lazy stream using FUNCTION and SEED.
FUNCTION takes a seed and returns (value . next-seed) or nil to stop."
  (lazy
   (let ((result (funcall function seed)))
     (if (null result)
         (lazy-nil)
       (lazy-cons (car result)
                 (lazy-seq-unfold function (cdr result)))))))

(defun lazy-seq-random (&optional limit)
  "Return an infinite lazy stream of random numbers.
If LIMIT is provided, returns random integers in [0, LIMIT).
Otherwise returns random floats in [0.0, 1.0)."
  (if limit
      (lazy-repeatedly (lambda () (random limit)))
    (lazy-repeatedly (lambda () (/ (float (random 1000000)) 1000000.0)))))

(defun lazy-seq-powers (base &optional start)
  "Return an infinite lazy stream of powers of BASE.
START defaults to 0, so the stream is: BASE^0, BASE^1, BASE^2, ..."
  (lazy-iterate (lambda (x) (* x base))
                (expt base (or start 0))))

(defun lazy-seq-lines (file)
  "Return a lazy stream of lines from FILE."
  (let ((buffer (find-file-noselect file)))
    (lazy-seq-unfold
     (lambda (buf)
       (with-current-buffer buf
         (unless (eobp)
           (let ((line (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))))
             (forward-line 1)
             (cons line buf)))))
     buffer)))

(defun lazy-seq-repeat (n &rest values)
  "Return a lazy stream that repeats VALUES exactly N times.
If N is nil, repeats infinitely."
  (if (null n)
      (lazy-cycle (lazy-seq-from-list values))
    (lazy
     (if (<= n 0)
         (lazy-nil)
       (lazy-append (lazy-seq-from-list values)
                   (apply #'lazy-seq-repeat (1- n) values))))))

(defun lazy-seq-take-while (pred source)
  "Return a lazy stream from SOURCE while PRED hold.
SOURCE can be a list, vector, or string."
  (cond
   ((listp source)
    (lazy-take-while pred (lazy-seq-from-list source)))
   ((vectorp source)
    (lazy-take-while pred (lazy-seq-from-list (append source nil))))
   ((stringp source)
    (lazy-take-while pred (lazy-seq-from-list (string-to-list source))))
   (t (error "Unsupported source type"))))

(provide 'lazy-seq)
;;; lazy-seq.el ends here
