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

(defun lazy-box (x)
  "Box X into a mutable container."
  (list x))

(defalias 'lazy-unbox 'car
  "Unbox a value from its container.")

(defalias 'lazy-setbox 'setcar
  "Set the value in a boxed container.")

(defmacro lazy (&rest body)
  "Create a lazy promise that will evaluate BODY when forced."
  `(lazy-box (cons 'lazy (lambda () ,@body))))

(defsubst lazy-eager (x)
  "Create an eager promise wrapping value X."
  (lazy-box (cons 'lazy-eager x)))

(defmacro lazy-delay (&rest body)
  "Delay evaluation of BODY until forced."
  `(lazy (lazy-eager (progn ,@body))))

(defun lazy-force (promise)
  "Force evaluation of PROMISE and return its value."
  (while (let ((content (lazy-unbox promise)))
           (cl-case (car content)
             (lazy-eager (setq promise (cdr content))
                         nil)
             (lazy (let* ((promise* (funcall (cdr content)))
                          (content  (lazy-unbox promise)))    ; *
                     (if (not (eq (car content) 'lazy-eager)) ; *
                         (progn (setcar content (car (lazy-unbox promise*)))
                                (setcdr content (cdr (lazy-unbox promise*)))
                                (lazy-setbox promise* content))))
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
;;       (let ((p (lazy-delay (if (<= count 0)
;;                              count
;;                            (progn (setq count (- count 1))
;;                                   (lazy-force p)
;;                                   (setq count (+ count 2))
;;                                   count)))))
;;         (list #'get-count p)))))
;; (defvar get-count (car q))
;; (defvar p (cadr q))
;;
;; (funcall get-count)                     ; =>   5
;; (lazy-force p)                            ; =>   0
;; (funcall get-count)                     ; =>   10


;;; Stream functions

(defmacro lazy-cons (first rest)
  "Construct a lazy stream with FIRST element and REST stream."
  `(lazy-delay (cons ,first ,rest)))

(defsubst lazy-nil ()
  "Return an empty stream."
  (lazy-delay nil))

(defsubst lazy-null (stream)
  "Return t if STREAM is empty."
  (null (lazy-force stream)))

(defsubst lazy-car (stream)
  "Return the first element of STREAM."
  (car (lazy-force stream)))

(defsubst lazy-cdr (stream)
  "Return the rest of STREAM after the first element."
  (cdr (lazy-force stream)))

(defalias 'lazy-empty 'lazy-nil
  "Alias for `lazy-nil'.")

(defalias 'lazy-empty-p 'lazy-null
  "Alias for `lazy-null'.")

(defalias 'lazy-first 'lazy-car
  "Alias for `lazy-car'.")

(defalias 'lazy-rest 'lazy-cdr
  "Alias for `lazy-cdr'.")

(defun lazy-append (&rest streams)
  "Append STREAMS into a single lazy stream."
  (if (null streams)
      (lazy-nil)
    (lazy
     (let ((first (pop streams)))
       (while (and (lazy-null first) streams)
         (setq first (pop streams)))
       (if (lazy-null first)
           (lazy-nil)
         (lazy-cons (lazy-car first)
                    (if streams (apply #'lazy-append (lazy-cdr first) streams)
                      (lazy-cdr first))))))))

(defmacro lazy-pop (stream)
  "Pop and return the first element of STREAM, modifying STREAM."
  (unless (symbolp stream)
    (error "STREAM must be a symbol"))
  `(prog1 (lazy-car ,stream)
     (setq ,stream (lazy-cdr ,stream))))

(defun lazy-elt (stream n)
  "Return the Nth element of STREAM (0-indexed)."
  (while (> n 0)
    (setq stream (lazy-cdr stream))
    (setq n (1- n)))
  (lazy-car stream))

(defun lazy-length (stream)
  "Return the length of STREAM."
  (if (null stream)
      0
    (let ((len 0))
      (while (not (lazy-null stream))
        (setq len (1+ len)
              stream (lazy-cdr stream)))
      len)))

(defun lazy-stream-p (stream)
  "Return t if STREAM is a lazy stream."
  (and (consp stream)
       (consp (car stream))
       (memq (caar stream) '(lazy lazy-eager))
       t))

(defun lazy-do (function stream)
  "Apply FUNCTION to each element of STREAM for side effects."
  (while (not (lazy-null stream))
    (funcall function (lazy-car stream))
    (setq stream (lazy-cdr stream))))

(defalias 'lazy-each #'lazy-do
  "Alias for `lazy-do'.")

(defmacro lazy-dostream (spec &rest body)
  "Iterate over STREAM in SPEC, executing BODY for each element."
  (declare (indent 1))
  `(lazy-do (lambda (,(car spec))
              ,@body)
            ,(cadr spec)))

(defun lazy-into-list (stream)
  "Convert STREAM into a list."
  (let (list)
    (lazy-dostream (elt stream)
      (push elt list))
    (nreverse list)))

(defun lazy-take (stream n)
  "Take the first N elements from STREAM."
  (when (< n 0) (setq n 0))
  (lazy
   (if (or (zerop n)
           (lazy-null stream))
       (lazy-nil)
     (lazy-cons (lazy-car stream)
                (lazy-take (lazy-cdr stream) (1- n))))))

(defun lazy-drop (stream n)
  "Drop the first N elements from STREAM and return the rest."
  (when (< n 0) (setq n 0))
  (if (null stream)
      (lazy-nil)
    (lazy
     (while (not (or (lazy-null stream)
                     (zerop n)))
       (setq n (1- n))
       (setq stream (lazy-cdr stream)))
     (if (lazy-null stream)
         (lazy-nil)
       (lazy-cons (lazy-car stream) (lazy-cdr stream))))))

(defun lazy-take-while (pred stream)
  "Take elements from STREAM while PRED hold."
  (if (or (null stream) (lazy-null stream))
      (lazy-nil)
    (lazy
     (if (not (funcall pred (lazy-car stream)))
         (lazy-nil)
       (lazy-cons (lazy-car stream)
                  (lazy-take-while pred (lazy-cdr stream)))))))

(defun lazy-drop-while (pred stream)
  "Drop elements from STREAM while PRED hold."
  (if (null stream)
      (lazy-nil)
    (lazy
     (while (and (not (lazy-null stream))
                 (funcall pred (lazy-car stream)))
       (setq stream (lazy-cdr stream)))
     (if (lazy-null stream)
         (lazy-nil)
       (lazy-cons (lazy-car stream)
                  (lazy-cdr stream))))))

(defun lazy-subseq (stream start &optional end)
  "Return a subsequence of STREAM from START to END."
  (when (or (< start 0) (and end (< end 0)))
    (error "Lazy-subseq: only non-negative indexes allowed for streams"))
  (let ((stream-from-start (lazy-drop stream start)))
    (if end
        (lazy-take stream-from-start (- end start))
      stream-from-start)))

(defun lazy-map (function stream)
  "Apply FUNCTION to each element of STREAM, returning a new stream."
  (lazy
   (if (lazy-null stream)
       (lazy-nil)
     (lazy-cons (funcall function (lazy-car stream))
                (lazy-map function (lazy-cdr stream))))))

(defun lazy-mapn (function stream &rest streams)
  "Apply FUNCTION to elements from STREAM and STREAMS in parallel."
  (setq streams (cons stream streams))
  (lazy
   (if (not (cl-every (lambda (x) (not (lazy-null x)))
                      streams))
       (lazy-nil)
     (lazy-cons (apply function (mapcar #'lazy-car streams))
                (apply #'lazy-mapn function (mapcar #'lazy-cdr streams))))))

(defun lazy-filter (pred stream)
  "Filter STREAM to elements where PRED hold."
  (lazy
   (while (not (or (lazy-null stream)
                   (funcall pred (lazy-car stream))))
     (setq stream (lazy-cdr stream)))
   (if (lazy-null stream)
       (lazy-nil)
     (lazy-cons (lazy-car stream)
                (lazy-filter pred (lazy-cdr stream))))))

(defun lazy-remove (pred stream)
  "Remove elements from STREAM where PRED hold."
  (lazy-filter (lambda (elt) (not (funcall pred elt)))
               stream))

(defun lazy-reduce (function stream initial-value)
  "Reduce STREAM using FUNCTION with INITIAL-VALUE as accumulator."
  (if (lazy-null stream)
      initial-value
    (let ((acc initial-value))
      (lazy-dostream (elt stream)
        (setq acc (funcall function acc elt)))
      acc)))

(defun lazy-reduce-while (pred function stream initial-value)
  "Reduce STREAM with FUNCTION while PRED hold."
  (if (lazy-null stream)
      initial-value
    (let ((acc initial-value))
      (catch 'lazy--break
        (lazy-dostream (elt stream)
          (setq acc (funcall function acc elt))
          (unless (funcall pred acc)
            (throw 'lazy--break nil))))
      acc)))

(defun lazy-find (pred stream &optional default)
  "Find the first element in STREAM where PRED hold.
Return DEFAULT if not found."
  (catch 'lazy--break
    (lazy-dostream (elt stream)
      (when (funcall pred elt)
        (throw 'lazy--break elt)))
    default))

(defun lazy-concat (streams)
  "Concatenate a stream of STREAMS into a single stream."
  (lazy
   (if (lazy-null streams)
       (lazy-nil)
     (let ((first (lazy-car streams)))
       (if (lazy-null first)
           (lazy-concat (lazy-cdr streams))
         (lazy-cons (lazy-car first)
                   (lazy-concat (lazy-cons (lazy-cdr first)
                                          (lazy-cdr streams)))))))))

(defun lazy-interleave (&rest streams)
  "Interleave elements from STREAMS."
  (setq streams (cl-remove-if #'lazy-null streams))
  (lazy
   (if (null streams)
       (lazy-nil)
     (lazy-cons (lazy-car (car streams))
               (apply #'lazy-interleave
                      (append (cdr streams)
                              (list (lazy-cdr (car streams)))))))))

(defun lazy-cycle (stream)
  "Repeat STREAM infinitely."
  (let ((original stream))
    (lazy
     (cl-labels ((cycle-helper (s)
                   (if (lazy-null s)
                       (cycle-helper original)
                     (lazy-cons (lazy-car s)
                               (cycle-helper (lazy-cdr s))))))
       (cycle-helper stream)))))

(defun lazy-repeat (x)
  "Create an infinite stream of X."
  (lazy (lazy-cons x (lazy-repeat x))))

(defun lazy-repeatedly (function)
  "Create an infinite stream by calling FUNCTION repeatedly."
  (lazy (lazy-cons (funcall function)
                  (lazy-repeatedly function))))

(defun lazy-iterate (function x)
  "Create an infinite stream by applying FUNCTION to X repeatedly."
  (lazy (lazy-cons x (lazy-iterate function (funcall function x)))))

(defun lazy-distinct (stream)
  "Remove duplicate elements from STREAM."
  (let ((seen (make-hash-table :test 'equal)))
    (lazy-filter (lambda (elt)
                  (unless (gethash elt seen)
                    (puthash elt t seen)
                    t))
                stream)))

(defun lazy-dedupe (stream)
  "Remove consecutive duplicate elements from STREAM."
  (lazy
   (if (lazy-null stream)
       (lazy-nil)
     (let ((first (lazy-car stream)))
       (lazy-cons first
                 (lazy-dedupe (lazy-drop-while
                              (lambda (x) (equal x first))
                              (lazy-cdr stream))))))))

(defun lazy-reductions (function stream initial-value)
  "Return a stream of successive reductions of STREAM with FUNCTION."
  (lazy
   (if (lazy-null stream)
       (lazy-cons initial-value (lazy-nil))
     (lazy-cons initial-value
               (lazy-reductions function
                              (lazy-cdr stream)
                              (funcall function initial-value
                                      (lazy-car stream)))))))

(defun lazy-split-at (stream n)
  "Split STREAM at position N, returning (BEFORE . AFTER)."
  (cons (lazy-take stream n)
        (lazy-drop stream n)))

(defun lazy-split-with (pred stream)
  "Split STREAM where PRED change from true to false."
  (cons (lazy-take-while pred stream)
        (lazy-drop-while pred stream)))

(defun lazy-map-indexed (function stream)
  "Apply FUNCTION to index and each element of STREAM."
  (let ((index -1))
    (lazy-map (lambda (elt)
               (setq index (1+ index))
               (funcall function index elt))
             stream)))

(defun lazy-take-nth (stream n)
  "Take every Nth element from STREAM."
  (when (< n 1) (error "N must be positive"))
  (lazy
   (if (lazy-null stream)
       (lazy-nil)
     (lazy-cons (lazy-car stream)
               (lazy-take-nth (lazy-drop stream n) n)))))

(defun lazy-some (pred stream)
  "Return the first truthy result of PRED applied to STREAM elements."
  (catch 'lazy--break
    (lazy-dostream (elt stream)
      (let ((result (funcall pred elt)))
        (when result
          (throw 'lazy--break result))))
    nil))

(defun lazy-every (pred stream)
  "Return t if PRED hold for all elements of STREAM."
  (catch 'lazy--break
    (lazy-dostream (elt stream)
      (unless (funcall pred elt)
        (throw 'lazy--break nil)))
    t))

(defun lazy-keep (function stream)
  "Apply FUNCTION to elements of STREAM and keep non-nil results."
  (lazy
   (if (lazy-null stream)
       (lazy-nil)
     (let ((result (funcall function (lazy-car stream))))
       (if result
           (lazy-cons result (lazy-keep function (lazy-cdr stream)))
         (lazy-keep function (lazy-cdr stream)))))))

(defun lazy-partition (stream n)
  "Partition STREAM into chunks of size N."
  (when (< n 1) (error "N must be positive"))
  (lazy
   (let ((chunk (lazy-into-list (lazy-take stream n))))
     (if (= (length chunk) n)
         (lazy-cons chunk (lazy-partition (lazy-drop stream n) n))
       (lazy-nil)))))

(defun lazy-partition-by (function stream)
  "Partition STREAM when FUNCTION result change."
  (lazy
   (if (lazy-null stream)
       (lazy-nil)
     (let* ((first (lazy-car stream))
            (fval (funcall function first))
            (run (lazy-cons first
                           (lazy-take-while
                            (lambda (x) (equal (funcall function x) fval))
                            (lazy-cdr stream)))))
       (lazy-cons (lazy-into-list run)
                 (lazy-partition-by function
                                   (lazy-drop stream (lazy-length run))))))))

(defun lazy-flatten (stream)
  "Flatten one level of nesting in STREAM."
  (lazy
   (if (lazy-null stream)
       (lazy-nil)
     (let ((first (lazy-car stream)))
       (if (lazy-stream-p first)
           (lazy-append first (lazy-flatten (lazy-cdr stream)))
         (lazy-cons first (lazy-flatten (lazy-cdr stream))))))))

;; `cl-loop' support
;;
;; (cl-loop repeat 10 for i lazy-by (lazy-primes) collect i)
;; => (2 3 5 7 11 13 17 19 23 29)

(defvar cl--loop-args)

(defmacro lazy--advance-for (conscell)
  "Advance CONSCELL to the next element in the lazy stream."
  `(progn
     (setcar ,conscell (lazy-car (cdr ,conscell)))
     (setcdr ,conscell (lazy-cdr (cdr ,conscell)))
     ,conscell))

(defmacro lazy--initialize-for (stream)
  "Initialize a cell for iterating over lazy STREAM."
  (let ((cs (gensym "lazy--loop-temp")))
    `(let ((,cs (cons nil ,stream)))
       (lazy--advance-for ,cs))))

(defun lazy--handle-loop-for (var)
  "Support `lazy-by' in `cl-loop' for VAR."
  (let ((stream (pop cl--loop-args)))
    (setf cl--loop-args
          (append `(for ,var in (lazy--initialize-for ,stream)
                        by 'lazy--advance-for)
                  cl--loop-args))))

(put 'lazy-by 'cl-loop-for-handler 'lazy--handle-loop-for)

(provide 'lazy)
;;; lazy.el ends here
