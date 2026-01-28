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
  "Construct a lazy coll with FIRST element and REST stream."
  `(lazy-delay (cons ,first ,rest)))

(defsubst lazy-nil ()
  "Return an empty stream."
  (lazy-delay nil))

(defsubst lazy-null (coll)
  "Return t if COLL is empty."
  (null (lazy-force coll)))

(defsubst lazy-car (coll)
  "Return the first element of COLL."
  (car (lazy-force coll)))

(defsubst lazy-cdr (coll)
  "Return the rest of COLL after the first element."
  (cdr (lazy-force coll)))

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
  (lazy-concat (lazy-from-seq streams)))

(defmacro lazy-pop (coll)
  "Pop and return the first element of COLL, modifying COLL."
  (unless (symbolp coll)
    (error "COLL must be a symbol"))
  `(prog1 (lazy-car ,coll)
     (setq ,coll (lazy-cdr ,coll))))

(defun lazy-elt (n coll)
  "Return the Nth element of COLL (0-indexed)."
  (while (> n 0)
    (setq coll (lazy-cdr coll))
    (setq n (1- n)))
  (lazy-car coll))

(defun lazy-second (coll)
  "Return the second element of COLL."
  (lazy-car (lazy-cdr coll)))

(defun lazy-length (coll)
  "Return the length of COLL."
  (if (null coll)
      0
    (let ((len 0))
      (while (not (lazy-null coll))
        (setq len (1+ len)
              coll (lazy-cdr coll)))
      len)))

(defun lazy-bounded-length (n coll)
  "Return N if COLL has at least N items, else the count of COLL."
  (if (null coll)
      0
    (let ((len 0))
      (while (and (< len n) (not (lazy-null coll)))
        (setq len (1+ len)
              coll (lazy-cdr coll)))
      len)))

(defun lazy-stream-p (coll)
  "Return t if COLL is a lazy stream."
  (and (consp coll)
       (consp (car coll))
       (memq (caar coll) '(lazy lazy-eager))
       t))

(defun lazy-do (function coll)
  "Apply FUNCTION to each element of COLL for side effects."
  (while (not (lazy-null coll))
    (funcall function (lazy-car coll))
    (setq coll (lazy-cdr coll))))

(defmacro lazy-dostream (spec &rest body)
  "Iterate over COLL in SPEC, executing BODY for each element."
  (declare (indent 1))
  `(lazy-do (lambda (,(car spec))
              ,@body)
            ,(cadr spec)))

(defun lazy-into-list (coll)
  "Convert COLL into a list."
  (let (list)
    (lazy-dostream (elt coll)
      (push elt list))
    (nreverse list)))

(defun lazy-take (n coll)
  "Take the first N elements from COLL."
  (when (< n 0) (setq n 0))
  (lazy
   (if (or (zerop n)
           (lazy-null coll))
       (lazy-nil)
     (lazy-cons (lazy-car coll)
                (lazy-take (1- n) (lazy-cdr coll))))))

(defun lazy-drop (n coll)
  "Drop the first N elements from COLL and return the rest."
  (when (< n 0) (setq n 0))
  (if (null coll)
      (lazy-nil)
    (lazy
     (while (not (or (lazy-null coll)
                     (zerop n)))
       (setq n (1- n))
       (setq coll (lazy-cdr coll)))
     (if (lazy-null coll)
         (lazy-nil)
       (lazy-cons (lazy-car coll) (lazy-cdr coll))))))

(defun lazy-take-while (pred coll)
  "Take elements from COLL while PRED hold."
  (if (or (null coll) (lazy-null coll))
      (lazy-nil)
    (lazy
     (if (not (funcall pred (lazy-car coll)))
         (lazy-nil)
       (lazy-cons (lazy-car coll)
                  (lazy-take-while pred (lazy-cdr coll)))))))

(defun lazy-drop-while (pred coll)
  "Drop elements from COLL while PRED hold."
  (if (null coll)
      (lazy-nil)
    (lazy
     (while (and (not (lazy-null coll))
                 (funcall pred (lazy-car coll)))
       (setq coll (lazy-cdr coll)))
     (if (lazy-null coll)
         (lazy-nil)
       (lazy-cons (lazy-car coll)
                  (lazy-cdr coll))))))

(defun lazy-butlast (coll)
  "Return COLL without the last element."
  (lazy
   (if (lazy-null coll)
       (lazy-nil)
     (let ((rest (lazy-cdr coll)))
       (if (lazy-null rest)
           (lazy-nil)
         (lazy-cons (lazy-car coll)
                    (lazy-butlast rest)))))))

(defun lazy-drop-last (n coll)
  "Drop the last N elements from COLL."
  (lazy
   (let ((ahead (lazy-drop n coll)))
     (cl-labels ((drop-helper (s a)
                   (if (lazy-null a)
                       (lazy-nil)
                     (lazy-cons (lazy-car s)
                                (drop-helper (lazy-cdr s)
                                             (lazy-cdr a))))))
       (drop-helper coll ahead)))))

(defun lazy-take-last (n coll)
  "Take the last N elements from COLL."
  (let ((all (lazy-into-list coll)))
    (lazy-from-seq (last all n))))

(defun lazy-subseq (start end coll)
  "Return a subsequence of COLL from START to END."
  (when (or (< start 0) (and end (< end 0)))
    (error "Lazy-subseq: only non-negative indexes allowed for streams"))
  (let ((stream-from-start (lazy-drop start coll)))
    (if end
        (lazy-take (- end start) stream-from-start)
      stream-from-start)))

(defun lazy-map (function coll)
  "Apply FUNCTION to each element of COLL, returning a new stream."
  (lazy
   (if (lazy-null coll)
       (lazy-nil)
     (lazy-cons (funcall function (lazy-car coll))
                (lazy-map function (lazy-cdr coll))))))

(defun lazy-mapcat (function coll)
  "Apply FUNCTION to each element of COLL and concatenate results."
  (lazy-flatten (lazy-map function coll)))

(defun lazy-mapn (function &rest streams)
  "Apply FUNCTION to elements from STREAMS in parallel."
  (lazy
   (if (not (cl-every (lambda (x) (not (lazy-null x)))
                      streams))
       (lazy-nil)
     (lazy-cons (apply function (mapcar #'lazy-car streams))
                (apply #'lazy-mapn function (mapcar #'lazy-cdr streams))))))

(defun lazy-filter (pred coll)
  "Filter COLL to elements where PRED hold."
  (lazy
   (while (not (or (lazy-null coll)
                   (funcall pred (lazy-car coll))))
     (setq coll (lazy-cdr coll)))
   (if (lazy-null coll)
       (lazy-nil)
     (lazy-cons (lazy-car coll)
                (lazy-filter pred (lazy-cdr coll))))))

(defun lazy-remove (pred coll)
  "Remove elements from COLL where PRED hold."
  (lazy-filter (lambda (elt) (not (funcall pred elt)))
               coll))

(defun lazy-reduce (function &rest args)
  "Reduce COLL using FUNCTION with ARGS.
With 2 args: (lazy-reduce function coll)
With 3 args: (lazy-reduce function initial-value coll)"
  (let ((coll (if (= (length args) 1)
                  (car args)
                (cadr args)))
        (initial-value (if (= (length args) 1)
                           nil
                         (car args)))
        (has-initial (= (length args) 2)))
    (cond
     ((and (not has-initial) (lazy-null coll))
      (funcall function))
     ((and (not has-initial) (lazy-null (lazy-cdr coll)))
      (lazy-car coll))
     (has-initial
      (if (lazy-null coll)
          initial-value
        (let ((acc initial-value))
          (lazy-dostream (elt coll)
            (setq acc (funcall function acc elt)))
          acc)))
     (t
      (let ((acc (lazy-car coll)))
        (setq coll (lazy-cdr coll))
        (lazy-dostream (elt coll)
          (setq acc (funcall function acc elt)))
        acc)))))

(defun lazy-reduce-while (pred function initial-value coll)
  "Reduce COLL with FUNCTION while PRED hold."
  (if (lazy-null coll)
      initial-value
    (let ((acc initial-value))
      (catch 'lazy--break
        (lazy-dostream (elt coll)
          (setq acc (funcall function acc elt))
          (unless (funcall pred acc)
            (throw 'lazy--break nil))))
      acc)))

(defun lazy-find (pred coll &optional default)
  "Find the first element in COLL where PRED hold.
Return DEFAULT if not found."
  (catch 'lazy--break
    (lazy-dostream (elt coll)
      (when (funcall pred elt)
        (throw 'lazy--break elt)))
    default))

(defun lazy-concat (streams)
  "Concatenate a coll of STREAMS into a single stream."
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

(defun lazy-interpose (separator coll)
  "Insert SEPARATOR between elements of COLL."
  (lazy
   (if (lazy-null coll)
       (lazy-nil)
     (let ((first (lazy-car coll))
           (rest (lazy-cdr coll)))
       (if (lazy-null rest)
           (lazy-cons first (lazy-nil))
         (lazy-cons first
                    (lazy-cons separator
                               (lazy-interpose separator rest))))))))

(defun lazy-cycle (coll)
  "Repeat COLL infinitely."
  (let ((original coll))
    (lazy
     (cl-labels ((cycle-helper (s)
                   (if (lazy-null s)
                       (cycle-helper original)
                     (lazy-cons (lazy-car s)
                                (cycle-helper (lazy-cdr s))))))
       (cycle-helper coll)))))

(defun lazy-repeat (x)
  "Create an infinite coll of X."
  (lazy (lazy-cons x (lazy-repeat x))))

(defun lazy-repeatedly (function)
  "Create an infinite coll by calling FUNCTION repeatedly."
  (lazy (lazy-cons (funcall function)
                   (lazy-repeatedly function))))

(defun lazy-iterate (function x)
  "Create an infinite coll by applying FUNCTION to X repeatedly."
  (lazy (lazy-cons x (lazy-iterate function (funcall function x)))))

(defun lazy-distinct (coll)
  "Remove duplicate elements from COLL."
  (let ((seen (make-hash-table :test 'equal)))
    (lazy-filter (lambda (elt)
                   (unless (gethash elt seen)
                     (puthash elt t seen)
                     t))
                 coll)))

(defun lazy-dedupe (coll)
  "Remove consecutive duplicate elements from COLL."
  (lazy
   (if (lazy-null coll)
       (lazy-nil)
     (let ((first (lazy-car coll)))
       (lazy-cons first
                  (lazy-dedupe (lazy-drop-while
                                (lambda (x) (equal x first))
                                (lazy-cdr coll))))))))

(defun lazy-reductions (function initial-value coll)
  "Return a coll of successive reductions of COLL.
Use FUNCTION and INITIAL-VALUE for the reduction."
  (lazy
   (if (lazy-null coll)
       (lazy-cons initial-value (lazy-nil))
     (lazy-cons initial-value
                (lazy-reductions function
                                 (funcall function initial-value
                                          (lazy-car coll))
                                 (lazy-cdr coll))))))

(defun lazy-split-at (n coll)
  "Split COLL at position N, returning (BEFORE . AFTER)."
  (cons (lazy-take n coll)
        (lazy-drop n coll)))

(defun lazy-split-with (pred coll)
  "Split COLL where PRED change from true to false."
  (cons (lazy-take-while pred coll)
        (lazy-drop-while pred coll)))

(defun lazy-map-indexed (function coll)
  "Apply FUNCTION to index and each element of COLL."
  (lazy-mapn function (lazy-range) coll))

(defun lazy-take-nth (n coll)
  "Take every Nth element from COLL."
  (when (< n 1) (error "N must be positive"))
  (lazy
   (if (lazy-null coll)
       (lazy-nil)
     (lazy-cons (lazy-car coll)
                (lazy-take-nth n (lazy-drop n coll))))))

(defun lazy-some (pred coll)
  "Return the first truthy result of PRED applied to COLL elements."
  (catch 'lazy--break
    (lazy-dostream (elt coll)
      (let ((result (funcall pred elt)))
        (when result
          (throw 'lazy--break result))))
    nil))

(defun lazy-every (pred coll)
  "Return t if PRED hold for all elements of COLL."
  (not (lazy-some (lambda (x) (not (funcall pred x))) coll)))

(defun lazy-keep (function coll)
  "Apply FUNCTION to elements of COLL and keep non-nil results."
  (lazy
   (if (lazy-null coll)
       (lazy-nil)
     (let ((result (funcall function (lazy-car coll))))
       (if result
           (lazy-cons result (lazy-keep function (lazy-cdr coll)))
         (lazy-keep function (lazy-cdr coll)))))))

(defun lazy-partition (n coll)
  "Partition COLL into chunks of size N."
  (when (< n 1) (error "N must be positive"))
  (lazy
   (let ((chunk (lazy-into-list (lazy-take n coll))))
     (if (= (length chunk) n)
         (lazy-cons chunk (lazy-partition n (lazy-drop n coll)))
       (lazy-nil)))))

(defun lazy-partition-by (function coll)
  "Partition COLL when FUNCTION result change."
  (lazy
   (if (lazy-null coll)
       (lazy-nil)
     (let* ((first (lazy-car coll))
            (fval (funcall function first))
            (run (lazy-cons first
                            (lazy-take-while
                             (lambda (x) (equal (funcall function x) fval))
                             (lazy-cdr coll)))))
       (lazy-cons (lazy-into-list run)
                  (lazy-partition-by function
                                     (lazy-drop (lazy-length run) coll)))))))

(defun lazy-group-by (function coll)
  "Group elements of COLL by the result of FUNCTION.
Return an alist of (key . list-of-elements)."
  (let ((groups (make-hash-table :test 'equal)))
    (lazy-dostream (elt coll)
      (let* ((key (funcall function elt))
             (existing (gethash key groups)))
        (puthash key (cons elt existing) groups)))
    (let (result)
      (maphash (lambda (k v)
                 (push (cons k (reverse v)) result))
               groups)
      result)))

(defun lazy-flatten (coll)
  "Flatten one level of nesting in COLL."
  (lazy
   (if (lazy-null coll)
       (lazy-nil)
     (let ((first (lazy-car coll)))
       (if (lazy-stream-p first)
           (lazy-append first (lazy-flatten (lazy-cdr coll)))
         (lazy-cons first (lazy-flatten (lazy-cdr coll))))))))

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

(defmacro lazy--initialize-for (coll)
  "Initialize a cell for iterating over lazy COLL."
  (let ((cs (gensym "lazy--loop-temp")))
    `(let ((,cs (cons nil ,coll)))
       (lazy--advance-for ,cs))))

(defun lazy--handle-loop-for (var)
  "Support `lazy-by' in `cl-loop' for VAR."
  (let ((coll (pop cl--loop-args)))
    (setf cl--loop-args
          (append `(for ,var in (lazy--initialize-for ,coll)
                        by 'lazy--advance-for)
                  cl--loop-args))))

(put 'lazy-by 'cl-loop-for-handler 'lazy--handle-loop-for)


;;;
;;; Sequence generators

(defun lazy-range (&optional start end step)
  "Create a lazy coll of numbers from START to END by STEP."
  (unless start (setq start 0))
  (and end (> start end) (setq end start))
  (unless step (setq step 1))
  (lazy
   (if (and end (= start end))
       (lazy-nil)
     (lazy-cons start (lazy-range (+ start step) end step)))))

(defun lazy--sieve (coll)
  "Sieve of Eratosthenes for COLL."
  (lazy
   (lazy-cons (lazy-car coll)
              (lazy--sieve (lazy-filter (lambda (x)
                                          (/= 0 (% x (lazy-car coll))))
                                        (lazy-cdr coll))))))

(defun lazy-primes ()
  "Return an infinite lazy coll of prime numbers."
  (lazy--sieve (lazy-range 2)))

(defun lazy-fibonacci ()
  "Return an infinite lazy coll of Fibonacci numbers."
  (lazy
   (cl-labels ((rec (a b)
                 (lazy-cons (+ a b)
                            (rec b (+ a b)))))
     (lazy-cons 0 (lazy-cons 1 (rec 0 1))))))

(defun lazy-from-seq (seq &optional pred)
  "Convert SEQ to a lazy stream.
SEQ can be any sequence type (list, vector, string, etc.).
If PRED is provided, take elements while PRED holds."
  (let ((coll (cl-typecase seq
                (list
                 (if (null seq)
                     (lazy-nil)
                   (lazy-cons (car seq)
                              (lazy-from-seq (cdr seq)))))
                (t
                 (let ((len (length seq))
                       (idx 0))
                   (lazy
                    (cl-labels ((helper (i)
                                  (if (>= i len)
                                      (lazy-nil)
                                    (lazy-cons (elt seq i)
                                               (helper (1+ i))))))
                      (helper idx))))))))
    (if pred
        (lazy-take-while pred coll)
      coll)))

(defun lazy-unfold (function seed)
  "Return an infinite lazy coll using FUNCTION and SEED.
FUNCTION takes a seed and returns (value . next-seed) or nil to stop."
  (lazy
   (let ((result (funcall function seed)))
     (if (null result)
         (lazy-nil)
       (lazy-cons (car result)
                  (lazy-unfold function (cdr result)))))))

(defun lazy-random (&optional limit)
  "Return an infinite lazy coll of random numbers.
If LIMIT is provided, returns random integers in [0, LIMIT).
Otherwise returns random floats in [0.0, 1.0)."
  (if limit
      (lazy-repeatedly (lambda () (random limit)))
    (lazy-repeatedly (lambda () (/ (float (random 1000000)) 1000000.0)))))

(defun lazy-powers (base &optional start)
  "Return an infinite lazy coll of powers of BASE.
START defaults to 0, so the coll is: BASE^0, BASE^1, BASE^2, ..."
  (lazy-iterate (lambda (x) (* x base))
                (expt base (or start 0))))

(defun lazy-lines (file)
  "Return a lazy coll of lines from FILE."
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


(provide 'lazy)
;;; lazy.el ends here
