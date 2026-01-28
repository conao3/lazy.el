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
  (lazy-concat (lazy-from-list streams)))

(defmacro lazy-pop (stream)
  "Pop and return the first element of STREAM, modifying STREAM."
  (unless (symbolp stream)
    (error "STREAM must be a symbol"))
  `(prog1 (lazy-car ,stream)
     (setq ,stream (lazy-cdr ,stream))))

(defun lazy-elt (n stream)
  "Return the Nth element of STREAM (0-indexed)."
  (while (> n 0)
    (setq stream (lazy-cdr stream))
    (setq n (1- n)))
  (lazy-car stream))

(defun lazy-second (stream)
  "Return the second element of STREAM."
  (lazy-car (lazy-cdr stream)))

(defun lazy-length (stream)
  "Return the length of STREAM."
  (if (null stream)
      0
    (let ((len 0))
      (while (not (lazy-null stream))
        (setq len (1+ len)
              stream (lazy-cdr stream)))
      len)))

(defun lazy-bounded-length (n stream)
  "Return N if STREAM has at least N items, else the count of STREAM."
  (if (null stream)
      0
    (let ((len 0))
      (while (and (< len n) (not (lazy-null stream)))
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

(defun lazy-take (n stream)
  "Take the first N elements from STREAM."
  (when (< n 0) (setq n 0))
  (lazy
   (if (or (zerop n)
           (lazy-null stream))
       (lazy-nil)
     (lazy-cons (lazy-car stream)
                (lazy-take (1- n) (lazy-cdr stream))))))

(defun lazy-drop (n stream)
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

(defun lazy-butlast (stream)
  "Return STREAM without the last element."
  (lazy
   (if (lazy-null stream)
       (lazy-nil)
     (let ((rest (lazy-cdr stream)))
       (if (lazy-null rest)
           (lazy-nil)
         (lazy-cons (lazy-car stream)
                   (lazy-butlast rest)))))))

(defun lazy-drop-last (n stream)
  "Drop the last N elements from STREAM."
  (lazy
   (let ((ahead (lazy-drop n stream)))
     (cl-labels ((drop-helper (s a)
                   (if (lazy-null a)
                       (lazy-nil)
                     (lazy-cons (lazy-car s)
                               (drop-helper (lazy-cdr s)
                                           (lazy-cdr a))))))
       (drop-helper stream ahead)))))

(defun lazy-take-last (n stream)
  "Take the last N elements from STREAM."
  (let ((all (lazy-into-list stream)))
    (lazy-from-list (last all n))))

(defun lazy-subseq (start end stream)
  "Return a subsequence of STREAM from START to END."
  (when (or (< start 0) (and end (< end 0)))
    (error "Lazy-subseq: only non-negative indexes allowed for streams"))
  (let ((stream-from-start (lazy-drop start stream)))
    (if end
        (lazy-take (- end start) stream-from-start)
      stream-from-start)))

(defun lazy-map (function stream)
  "Apply FUNCTION to each element of STREAM, returning a new stream."
  (lazy
   (if (lazy-null stream)
       (lazy-nil)
     (lazy-cons (funcall function (lazy-car stream))
                (lazy-map function (lazy-cdr stream))))))

(defun lazy-mapcat (function stream)
  "Apply FUNCTION to each element of STREAM and concatenate results."
  (lazy-flatten (lazy-map function stream)))

(defun lazy-mapn (function &rest streams)
  "Apply FUNCTION to elements from STREAMS in parallel."
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

(defun lazy-reduce (function &rest args)
  "Reduce STREAM using FUNCTION with ARGS.
With 2 args: (lazy-reduce function stream)
With 3 args: (lazy-reduce function initial-value stream)"
  (let ((stream (if (= (length args) 1)
                    (car args)
                  (cadr args)))
        (initial-value (if (= (length args) 1)
                          nil
                        (car args)))
        (has-initial (= (length args) 2)))
    (cond
     ((and (not has-initial) (lazy-null stream))
      (funcall function))
     ((and (not has-initial) (lazy-null (lazy-cdr stream)))
      (lazy-car stream))
     (has-initial
      (if (lazy-null stream)
          initial-value
        (let ((acc initial-value))
          (lazy-dostream (elt stream)
            (setq acc (funcall function acc elt)))
          acc)))
     (t
      (let ((acc (lazy-car stream)))
        (setq stream (lazy-cdr stream))
        (lazy-dostream (elt stream)
          (setq acc (funcall function acc elt)))
        acc)))))

(defun lazy-reduce-while (pred function initial-value stream)
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

(defun lazy-interpose (separator stream)
  "Insert SEPARATOR between elements of STREAM."
  (lazy
   (if (lazy-null stream)
       (lazy-nil)
     (let ((first (lazy-car stream))
           (rest (lazy-cdr stream)))
       (if (lazy-null rest)
           (lazy-cons first (lazy-nil))
         (lazy-cons first
                   (lazy-cons separator
                             (lazy-interpose separator rest))))))))

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

(defun lazy-reductions (function initial-value stream)
  "Return a stream of successive reductions of STREAM.
Use FUNCTION and INITIAL-VALUE for the reduction."
  (lazy
   (if (lazy-null stream)
       (lazy-cons initial-value (lazy-nil))
     (lazy-cons initial-value
               (lazy-reductions function
                              (funcall function initial-value
                                      (lazy-car stream))
                              (lazy-cdr stream))))))

(defun lazy-split-at (n stream)
  "Split STREAM at position N, returning (BEFORE . AFTER)."
  (cons (lazy-take n stream)
        (lazy-drop n stream)))

(defun lazy-split-with (pred stream)
  "Split STREAM where PRED change from true to false."
  (cons (lazy-take-while pred stream)
        (lazy-drop-while pred stream)))

(defun lazy-map-indexed (function stream)
  "Apply FUNCTION to index and each element of STREAM."
  (lazy-mapn function (lazy-range) stream))

(defun lazy-take-nth (n stream)
  "Take every Nth element from STREAM."
  (when (< n 1) (error "N must be positive"))
  (lazy
   (if (lazy-null stream)
       (lazy-nil)
     (lazy-cons (lazy-car stream)
               (lazy-take-nth n (lazy-drop n stream))))))

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
  (not (lazy-some (lambda (x) (not (funcall pred x))) stream)))

(defun lazy-keep (function stream)
  "Apply FUNCTION to elements of STREAM and keep non-nil results."
  (lazy
   (if (lazy-null stream)
       (lazy-nil)
     (let ((result (funcall function (lazy-car stream))))
       (if result
           (lazy-cons result (lazy-keep function (lazy-cdr stream)))
         (lazy-keep function (lazy-cdr stream)))))))

(defun lazy-partition (n stream)
  "Partition STREAM into chunks of size N."
  (when (< n 1) (error "N must be positive"))
  (lazy
   (let ((chunk (lazy-into-list (lazy-take n stream))))
     (if (= (length chunk) n)
         (lazy-cons chunk (lazy-partition n (lazy-drop n stream)))
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
                                   (lazy-drop (lazy-length run) stream)))))))

(defun lazy-group-by (function stream)
  "Group elements of STREAM by the result of FUNCTION.
Return an alist of (key . list-of-elements)."
  (let ((groups (make-hash-table :test 'equal)))
    (lazy-dostream (elt stream)
      (let* ((key (funcall function elt))
             (existing (gethash key groups)))
        (puthash key (cons elt existing) groups)))
    (let (result)
      (maphash (lambda (k v)
                 (push (cons k (reverse v)) result))
               groups)
      result)))

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


;;;
;;; Sequence generators

(defun lazy-range (&optional start end step)
  "Create a lazy stream of numbers from START to END by STEP."
  (unless start (setq start 0))
  (and end (> start end) (setq end start))
  (unless step (setq step 1))
  (lazy
   (if (and end (= start end))
       (lazy-nil)
     (lazy-cons start (lazy-range (+ start step) end step)))))

(defun lazy--sieve (stream)
  "Sieve of Eratosthenes for STREAM."
  (lazy
   (lazy-cons (lazy-car stream)
              (lazy--sieve (lazy-filter (lambda (x)
                                          (/= 0 (% x (lazy-car stream))))
                                        (lazy-cdr stream))))))

(defun lazy-primes ()
  "Return an infinite lazy stream of prime numbers."
  (lazy--sieve (lazy-range 2)))

(defun lazy-fibonacci ()
  "Return an infinite lazy stream of Fibonacci numbers."
  (lazy
   (cl-labels ((rec (a b)
                 (lazy-cons (+ a b)
                            (rec b (+ a b)))))
     (lazy-cons 0 (lazy-cons 1 (rec 0 1))))))

(defun lazy-from-list (list)
  "Convert LIST to a lazy stream."
  (if (null list)
      (lazy-nil)
    (lazy-cons (car list)
               (lazy-from-list (cdr list)))))

(defun lazy-unfold (function seed)
  "Return an infinite lazy stream using FUNCTION and SEED.
FUNCTION takes a seed and returns (value . next-seed) or nil to stop."
  (lazy
   (let ((result (funcall function seed)))
     (if (null result)
         (lazy-nil)
       (lazy-cons (car result)
                 (lazy-unfold function (cdr result)))))))

(defun lazy-random (&optional limit)
  "Return an infinite lazy stream of random numbers.
If LIMIT is provided, returns random integers in [0, LIMIT).
Otherwise returns random floats in [0.0, 1.0)."
  (if limit
      (lazy-repeatedly (lambda () (random limit)))
    (lazy-repeatedly (lambda () (/ (float (random 1000000)) 1000000.0)))))

(defun lazy-powers (base &optional start)
  "Return an infinite lazy stream of powers of BASE.
START defaults to 0, so the stream is: BASE^0, BASE^1, BASE^2, ..."
  (lazy-iterate (lambda (x) (* x base))
                (expt base (or start 0))))

(defun lazy-lines (file)
  "Return a lazy stream of lines from FILE."
  (let ((buffer (find-file-noselect file)))
    (lazy-unfold
     (lambda (buf)
       (with-current-buffer buf
         (unless (eobp)
           (let ((line (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))))
             (forward-line 1)
             (cons line buf)))))
     buffer)))

(defun lazy-from-source (pred source)
  "Return a lazy stream from SOURCE while PRED hold.
SOURCE can be a list, vector, or string."
  (cond
   ((listp source)
    (lazy-take-while pred (lazy-from-list source)))
   ((vectorp source)
    (lazy-take-while pred (lazy-from-list (append source nil))))
   ((stringp source)
    (lazy-take-while pred (lazy-from-list (string-to-list source))))
   (t (error "Unsupported source type"))))

(provide 'lazy)
;;; lazy.el ends here
