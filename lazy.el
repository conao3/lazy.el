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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Primitives
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lz-box (x) (list x))
(defalias 'lz-unbox 'car)
(defalias 'lz-setbox 'setcar)

(defmacro lz-lazy (exp)
  `(lz-box (cons 'lz-lazy (lambda () ,exp))))

(defsubst lz-eager (x)
  (lz-box (cons 'lz-eager x)))

(defmacro lz-delay (exp)
  `(lz-lazy (lz-eager ,exp)))

(defun lz-force (promise)
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
;; (*) これらの 2 行は、let* の最初の行が force された場合に元の promise を
;;     再フェッチしてチェックします。これが起こる例については、以下の
;;     reentrancy test 3 を参照してください。
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Stream functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro lz-cons (first rest)
  `(lz-delay (cons ,first ,rest)))

(defsubst lz-nil ()
  (lz-delay nil))

(defsubst lz-null (stream)
  (null (lz-force stream)))

(defsubst lz-car (stream)
  (car (lz-force stream)))

(defsubst lz-cdr (stream)
  (cdr (lz-force stream)))

(defalias 'lz-empty 'lz-nil)
(defalias 'lz-empty-p 'lz-null)
(defalias 'lz-first 'lz-car)
(defalias 'lz-rest 'lz-cdr)

(defun lz-append (&rest streams)
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
  (unless (symbolp stream)
    (error "STREAM must be a symbol"))
  `(prog1
       (lz-car ,stream)
     (setq ,stream (lz-cdr ,stream))))

(defun lz-elt (stream n)
  (while (> n 0)
    (setq stream (lz-cdr stream))
    (setq n (1- n)))
  (lz-car stream))

(defun lz-length (stream)
  (let ((len 0))
    (while (not (lz-null stream))
      (setq len (1+ len)
            stream (lz-cdr stream)))
    len))

(defun lz-stream-p (stream)
  (and (consp stream)
       (consp (car stream))
       (memq (caar stream) '(lz-lazy lz-eager))
       t))

(defun lz-do (function stream)
  (while (not (lz-null stream))
    (funcall function (lz-car stream))
    (setq stream (lz-cdr stream))))

(defalias 'lz-each #'lz-do)

(defmacro lz-dostream (spec &rest body)
  (declare (indent 1))
  `(lz-do (lambda (,(car spec))
            ,@body)
          ,(cadr spec)))

(defun lz-into-list (stream)
  (let (list)
    (lz-dostream (elt stream)
      (push elt list))
    (nreverse list)))

(defun lz-range (&optional start end step)
  (unless start (setq start 0))
  (and end (> start end) (setq end start))
  (unless step (setq step 1))
  (lz-lazy
   (if (and end (= start end))
       (lz-nil)
     (lz-cons start (lz-range (+ start step) end step)))))

(defun lz-take (stream n)
  (when (< n 0) (setq n 0))
  (lz-lazy
   (if (or (zerop n)
           (lz-null stream))
       (lz-nil)
     (lz-cons (lz-car stream)
              (lz-take (lz-cdr stream) (1- n))))))

(defun lz-drop (stream n)
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
  (lz-lazy
   (if (not (funcall pred (lz-car stream)))
       (lz-nil)
     (lz-cons (lz-car stream)
              (lz-take-while pred (lz-cdr stream))))))

(defun lz-drop-while (pred stream)
  (lz-lazy
   (progn
     (while (not (or (lz-null stream)
                     (funcall pred (lz-car stream))))
       (setq stream (lz-cdr stream)))
     (unless (lz-null stream)
       (lz-cons (lz-car stream)
                (lz-cdr stream))))))

(defun lz-subseq (stream start &optional end)
  (when (or (< start 0) (and end (< end 0)))
    (error "lz-subseq: only non-negative indexes allowed for streams"))
  (let ((stream-from-start (lz-drop stream start)))
    (if end
        (lz-take stream-from-start (- end start))
      stream-from-start)))

(defun lz-map (function stream)
  (lz-lazy
   (if (lz-null stream)
       (lz-nil)
     (lz-cons (funcall function (lz-car stream))
              (lz-map function (lz-cdr stream))))))

(defun lz-mapn (function stream &rest streams)
  (setq streams (cons stream streams))
  (lz-lazy
   (if (not (cl-every (lambda (x) (not (lz-null x)))
                      streams))
       (lz-nil)
     (lz-cons (apply function (mapcar #'lz-car streams))
              (apply #'lz-mapn function (mapcar #'lz-cdr streams))))))

(defun lz-filter (pred stream)
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
  (lz-filter (lambda (elt) (not (funcall pred elt)))
             stream))

(defun lz-reduce (function stream initial-value)
  (if (lz-null stream)
      initial-value
    (let ((acc initial-value))
      (lz-dostream (elt stream)
        (setq acc (funcall function acc elt)))
      acc)))

(defun lz-reduce-while (pred function stream initial-value)
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
  (catch 'lz--break
    (lz-dostream (elt stream)
      (when (funcall pred elt)
        (throw 'lz--break elt)))
    default))

(defun lz--sieve (stream)
  "Sieve of Eratosthenes"
  (lz-lazy
   (lz-cons (lz-car stream)
            (lz--sieve (lz-filter (lambda (x)
                                    (/= 0 (% x (lz-car stream))))
                                  (lz-cdr stream))))))

(defun lz-primes ()
  (lz--sieve (lz-range 2)))

(defun lz-fibonacci ()
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
  `(progn
     (setcar ,conscell (lz-car (cdr ,conscell)))
     (setcdr ,conscell (lz-cdr (cdr ,conscell)))
     ,conscell))

(defmacro lz--initialize-for (stream)
  (let ((cs (gensym "lz--loop-temp")))
    `(let ((,cs (cons nil ,stream)))
       (lz--advance-for ,cs))))

(defun lz--handle-loop-for (var)
  "Support `lazy-by' in `cl-loop'."
  (let ((stream (pop cl--loop-args)))
    (setf cl--loop-args
          (append `(for ,var in (lz--initialize-for ,stream)
                        by 'lz--advance-for)
                  cl--loop-args))))

(put 'lazy-by 'cl-loop-for-handler 'lz--handle-loop-for)

(provide 'lazy)
;;; lazy.el ends here
