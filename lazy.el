;;; lazy.el --- Lazy evaluation library              -*- lexical-binding: t; -*-

;; Copyright (C) 2018  chuntaro
;; Copyright (C) 2026  conao3

;; Author: chuntaro <chuntaro@sakura-games.jp>
;; Maintainer: conao3 <conao3@gmail.com>
;; URL: https://github.com/chuntaro/lazy.el
;; Package-Requires: ((emacs "25"))
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

;; Implementation of `SRFI 45' in Emacs Lisp and Stream functions.
;;
;; SRFI 45
;; https://srfi.schemers.org/srfi-45/srfi-45.html
;; http://www.katch.ne.jp/~leque/translations/srfi-45/srfi-45j.html (Japanese)

;;; Code:

(require 'cl-lib)


;;; Primitives

(defun lz-box (x)
  "Box X into a mutable container."
  (list x))

(defalias 'lz-unbox 'car
  "Unbox a value from its container.")

(defalias 'lz-setbox 'setcar
  "Set the value in a boxed container.")

(defmacro lz-lazy (exp)
  "Create a lazy promise that will evaluate EXP when forced."
  `(lz-box (cons 'lz-lazy (lambda () ,exp))))

(defsubst lz-eager (x)
  "Create an eager promise wrapping value X."
  (lz-box (cons 'lz-eager x)))

(defmacro lz-delay (exp)
  "Delay evaluation of EXP until forced."
  `(lz-lazy (lz-eager ,exp)))

(defun lz-force (promise)
  "Force evaluation of PROMISE and return its value."
  (while (let ((content (lz-unbox promise)))
           (cl-case (car content)
             (lz-eager (setq promise (cdr content))
                       nil)
             (lz-lazy  (let* ((promise* (funcall (cdr content)))
                              (content  (lz-unbox promise)))    ; *
                         (if (not (eq (car content) 'lz-eager)) ; *
                             (progn (setcar content (car (lz-unbox promise*)))
                                    (setcdr content (cdr (lz-unbox promise*)))
                                    (lz-setbox promise* content))))
                       t))))
  promise)

;; (*) These two lines re-fetch and check the original promise in case
;;     the first line of the let* caused it to be forced.  For an example
;;     where this happens, see reentrancy test 3 below.
;;
;;=========================================================================
;; Reentrancy test 3: due to John Shutt
;;
;; (define q
;;   (let ((count 5))
;;     (define (get-count) count)
;;     (define p (delay (if (<= count 0)
;;                          count
;;                          (begin (set! count (- count 1))
;;                                 (force p)
;;                                 (set! count (+ count 2))
;;                                 count))))
;;     (list get-count p)))
;; (define get-count (car q))
;; (define p (cadr q))
;;
;; (get-count)  ; =>   5
;; (force p)    ; =>   0
;; (get-count)  ; =>   10
;;
;; Emacs Lisp version:
;; (defvar q
;;   (let ((count 5))
;;     (cl-labels ((get-count () count))
;;       (let ((p (lz-delay (if (<= count 0)
;;                              count
;;                            (progn (setq count (- count 1))
;;                                   (lz-force p)
;;                                   (setq count (+ count 2))
;;                                   count)))))
;;         (list #'get-count p)))))
;; (defvar get-count (car q))
;; (defvar p (cadr q))
;;
;; (funcall get-count)                     ; =>   5
;; (lz-force p)                            ; =>   0
;; (funcall get-count)                     ; =>   10


;;; Stream functions

(defmacro lz-cons (first rest)
  "Construct a lazy stream with FIRST element and REST stream."
  `(lz-delay (cons ,first ,rest)))

(defsubst lz-nil ()
  "Return an empty stream."
  (lz-delay nil))

(defsubst lz-null (stream)
  "Return t if STREAM is empty."
  (null (lz-force stream)))

(defsubst lz-car (stream)
  "Return the first element of STREAM."
  (car (lz-force stream)))

(defsubst lz-cdr (stream)
  "Return the rest of STREAM after the first element."
  (cdr (lz-force stream)))

(defalias 'lz-empty 'lz-nil
  "Alias for `lz-nil'.")

(defalias 'lz-empty-p 'lz-null
  "Alias for `lz-null'.")

(defalias 'lz-first 'lz-car
  "Alias for `lz-car'.")

(defalias 'lz-rest 'lz-cdr
  "Alias for `lz-cdr'.")

(defun lz-append (&rest streams)
  "Append STREAMS into a single lazy stream."
  (if (null streams)
      (lz-nil)
    (lz-lazy
     (let ((first (pop streams)))
       (while (and (lz-null first) streams)
         (setq first (pop streams)))
       (if (lz-null first)
           (lz-nil)
         (lz-cons (lz-car first)
                  (if streams (apply #'lz-append (lz-cdr first) streams)
                    (lz-cdr first))))))))

(defmacro lz-pop (stream)
  "Pop and return the first element of STREAM, modifying STREAM."
  (unless (symbolp stream)
    (error "STREAM must be a symbol"))
  `(prog1
       (lz-car ,stream)
     (setq ,stream (lz-cdr ,stream))))

(defun lz-elt (stream n)
  "Return the Nth element of STREAM (0-indexed)."
  (while (> n 0)
    (setq stream (lz-cdr stream))
    (setq n (1- n)))
  (lz-car stream))

(defun lz-length (stream)
  "Return the length of STREAM."
  (let ((len 0))
    (while (not (lz-null stream))
      (setq len (1+ len)
            stream (lz-cdr stream)))
    len))

(defun lz-stream-p (stream)
  "Return t if STREAM is a lazy stream."
  (and (consp stream)
       (consp (car stream))
       (memq (caar stream) '(lz-lazy lz-eager))
       t))

(defun lz-do (function stream)
  "Apply FUNCTION to each element of STREAM for side effects."
  (while (not (lz-null stream))
    (funcall function (lz-car stream))
    (setq stream (lz-cdr stream))))

(defalias 'lz-each #'lz-do
  "Alias for `lz-do'.")

(defmacro lz-dostream (spec &rest body)
  "Iterate over STREAM in SPEC, executing BODY for each element."
  (declare (indent 1))
  `(lz-do (lambda (,(car spec))
            ,@body)
          ,(cadr spec)))

(defun lz-into-list (stream)
  "Convert STREAM into a list."
  (let (list)
    (lz-dostream (elt stream)
      (push elt list))
    (nreverse list)))

(defun lz-range (&optional start end step)
  "Create a lazy stream of numbers from START to END by STEP."
  (unless start (setq start 0))
  (and end (> start end) (setq end start))
  (unless step (setq step 1))
  (lz-lazy
   (if (and end (= start end))
       (lz-nil)
     (lz-cons start (lz-range (+ start step) end step)))))

(defun lz-take (stream n)
  "Take the first N elements from STREAM."
  (when (< n 0) (setq n 0))
  (lz-lazy
   (if (or (zerop n)
           (lz-null stream))
       (lz-nil)
     (lz-cons (lz-car stream)
              (lz-take (lz-cdr stream) (1- n))))))

(defun lz-drop (stream n)
  "Drop the first N elements from STREAM and return the rest."
  (when (< n 0) (setq n 0))
  (lz-lazy
   (progn
     (while (not (or (lz-null stream)
                     (zerop n)))
       (setq n (1- n))
       (setq stream (lz-cdr stream)))
     (if (lz-null stream)
         (lz-nil)
       (lz-cons (lz-car stream) (lz-cdr stream))))))

(defun lz-take-while (pred stream)
  "Take elements from STREAM while PRED hold."
  (lz-lazy
   (if (not (funcall pred (lz-car stream)))
       (lz-nil)
     (lz-cons (lz-car stream)
              (lz-take-while pred (lz-cdr stream))))))

(defun lz-drop-while (pred stream)
  "Drop elements from STREAM while PRED hold."
  (lz-lazy
   (progn
     (while (not (or (lz-null stream)
                     (funcall pred (lz-car stream))))
       (setq stream (lz-cdr stream)))
     (unless (lz-null stream)
       (lz-cons (lz-car stream)
                (lz-cdr stream))))))

(defun lz-subseq (stream start &optional end)
  "Return a subsequence of STREAM from START to END."
  (when (or (< start 0) (and end (< end 0)))
    (error "Lz-subseq: only non-negative indexes allowed for streams"))
  (let ((stream-from-start (lz-drop stream start)))
    (if end
        (lz-take stream-from-start (- end start))
      stream-from-start)))

(defun lz-map (function stream)
  "Apply FUNCTION to each element of STREAM, returning a new stream."
  (lz-lazy
   (if (lz-null stream)
       (lz-nil)
     (lz-cons (funcall function (lz-car stream))
              (lz-map function (lz-cdr stream))))))

(defun lz-mapn (function stream &rest streams)
  "Apply FUNCTION to elements from STREAM and STREAMS in parallel."
  (setq streams (cons stream streams))
  (lz-lazy
   (if (not (cl-every (lambda (x) (not (lz-null x)))
                      streams))
       (lz-nil)
     (lz-cons (apply function (mapcar #'lz-car streams))
              (apply #'lz-mapn function (mapcar #'lz-cdr streams))))))

(defun lz-filter (pred stream)
  "Filter STREAM to elements where PRED hold."
  (lz-lazy
   (progn
     (while (not (or (lz-null stream)
                     (funcall pred (lz-car stream))))
       (setq stream (lz-cdr stream)))
     (if (lz-null stream)
         (lz-nil)
       (lz-cons (lz-car stream)
                (lz-filter pred (lz-cdr stream)))))))

(defun lz-remove (pred stream)
  "Remove elements from STREAM where PRED hold."
  (lz-filter (lambda (elt) (not (funcall pred elt)))
             stream))

(defun lz-reduce (function stream initial-value)
  "Reduce STREAM using FUNCTION with INITIAL-VALUE as accumulator."
  (if (lz-null stream)
      initial-value
    (let ((acc initial-value))
      (lz-dostream (elt stream)
        (setq acc (funcall function acc elt)))
      acc)))

(defun lz-reduce-while (pred function stream initial-value)
  "Reduce STREAM with FUNCTION while PRED hold."
  (if (lz-null stream)
      initial-value
    (let ((acc initial-value))
      (catch 'lz--break
        (lz-dostream (elt stream)
          (setq acc (funcall function acc elt))
          (unless (funcall pred acc)
            (throw 'lz--break nil))))
      acc)))

(defun lz-find (pred stream &optional default)
  "Find the first element in STREAM where PRED hold.
Return DEFAULT if not found."
  (catch 'lz--break
    (lz-dostream (elt stream)
      (when (funcall pred elt)
        (throw 'lz--break elt)))
    default))

(defun lz--sieve (stream)
  "Sieve of Eratosthenes for STREAM."
  (lz-lazy
   (lz-cons (lz-car stream)
            (lz--sieve (lz-filter (lambda (x)
                                    (/= 0 (% x (lz-car stream))))
                                  (lz-cdr stream))))))

(defun lz-primes ()
  "Return an infinite lazy stream of prime numbers."
  (lz--sieve (lz-range 2)))

(defun lz-fibonacci ()
  "Return an infinite lazy stream of Fibonacci numbers."
  (lz-lazy
   (cl-labels ((rec (a b)
                    (lz-cons (+ a b)
                             (rec b (+ a b)))))
     (lz-cons 0 (lz-cons 1 (rec 0 1))))))

;; `cl-loop' support
;;
;; (cl-loop repeat 10 for i lazy-by (lz-primes) collect i)
;; => (2 3 5 7 11 13 17 19 23 29)

(defvar cl--loop-args)

(defmacro lz--advance-for (conscell)
  "Advance CONSCELL to the next element in the lazy stream."
  `(progn
     (setcar ,conscell (lz-car (cdr ,conscell)))
     (setcdr ,conscell (lz-cdr (cdr ,conscell)))
     ,conscell))

(defmacro lz--initialize-for (stream)
  "Initialize a cell for iterating over lazy STREAM."
  (let ((cs (gensym "lz--loop-temp")))
    `(let ((,cs (cons nil ,stream)))
       (lz--advance-for ,cs))))

(defun lz--handle-loop-for (var)
  "Support `lazy-by' in `cl-loop' for VAR."
  (let ((stream (pop cl--loop-args)))
    (setf cl--loop-args
          (append `(for ,var in (lz--initialize-for ,stream)
                        by 'lz--advance-for)
                  cl--loop-args))))

(put 'lazy-by 'cl-loop-for-handler 'lz--handle-loop-for)

(provide 'lazy)
;;; lazy.el ends here
