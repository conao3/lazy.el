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

(provide 'lazy-seq)
;;; lazy-seq.el ends here
