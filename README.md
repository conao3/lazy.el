# lazy.el
Lazy evaluation library for Emacs Lisp

Implementation of `SRFI 45' in Emacs Lisp and Stream functions.

SRFI 45  
https://srfi.schemers.org/srfi-45/srfi-45.html  
http://www.katch.ne.jp/~leque/translations/srfi-45/srfi-45j.html (Japanese)

It is used in the following article.  
https://qiita.com/chuntaro/items/f0d82f32cf216d4fd3dc (Japanese)

# Usage

## Overview

lazy.el provides lazy evaluation for Emacs Lisp based on SRFI 45. It implements lazy sequences that compute their elements on demand.

### Basic Example

```elisp
;; Create an infinite stream of natural numbers
(setq naturals (lazy-range))

;; Take first 5 elements
(lazy-into-list (lazy-take 5 naturals))
;;=> (0 1 2 3 4)

;; Map and filter
(lazy-into-list
 (lazy-take 5
  (lazy-filter #'cl-evenp
   (lazy-map (lambda (x) (* x x))
    (lazy-range 1)))))
;;=> (4 16 36 64 100)
```

### Using with dash.el threading macros

lazy.el uses Clojure-style data-last argument order, making it perfect for dash.el's `->>` macro:

```elisp
(require 'dash)

;; Thread-last style
(->> (lazy-range 1)
     (lazy-map (lambda (x) (* x x)))
     (lazy-filter #'cl-evenp)
     (lazy-take 5)
     lazy-into-list)
;;=> (4 16 36 64 100)

;; More complex pipeline
(->> (lazy-range 1 100)
     (lazy-filter (lambda (x) (zerop (mod x 3))))
     (lazy-map (lambda (x) (* x x)))
     (lazy-take-while (lambda (x) (< x 1000)))
     (lazy-partition 3)
     lazy-into-list)
;;=> ((9 36 81) (144 225 324) (441 576 729))

;; Combining multiple streams
(->> (list (lazy-range 0 5)
           (lazy-range 10 15)
           (lazy-range 20 25))
     lazy-from-seq
     lazy-concat
     (lazy-take 10)
     lazy-into-list)
;;=> (0 1 2 3 4 10 11 12 13 14)
```

### Key Concepts

- **Lazy sequences** are only computed when needed
- **Infinite sequences** are possible (e.g., `lazy-range`, `lazy-repeat`)
- **Memoization** ensures each element is computed only once
- **Clojure-style API** with data-last argument order

## Creating Lazy-sequence

Functions that create lazy sequences from various sources.

### From numbers

#### `(lazy-range &optional start end step)`

Create a lazy sequence of numbers from START to END by STEP.

```elisp
(lazy-into-list (lazy-range 5))
;;=> (0 1 2 3 4)

(lazy-into-list (lazy-range 3 8))
;;=> (3 4 5 6 7)

(lazy-into-list (lazy-range 0 10 2))
;;=> (0 2 4 6 8)

;; Infinite stream
(lazy-into-list (lazy-take 5 (lazy-range)))
;;=> (0 1 2 3 4)
```

### From mathematical sequences

#### `(lazy-primes)`

Return an infinite lazy sequence of prime numbers.

```elisp
(lazy-into-list (lazy-take 10 (lazy-primes)))
;;=> (2 3 5 7 11 13 17 19 23 29)
```

#### `(lazy-fibonacci)`

Return an infinite lazy sequence of Fibonacci numbers.

```elisp
(lazy-into-list (lazy-take 10 (lazy-fibonacci)))
;;=> (0 1 1 2 3 5 8 13 21 34)
```

#### `(lazy-powers base &optional start)`

Return an infinite lazy sequence of powers of BASE.

```elisp
(lazy-into-list (lazy-take 5 (lazy-powers 2)))
;;=> (1 2 4 8 16)

(lazy-into-list (lazy-take 4 (lazy-powers 3 2)))
;;=> (9 27 81 243)
```

### From sequences

#### `(lazy-from-seq seq &optional pred)`

Convert SEQ to a lazy sequence. SEQ can be any sequence type (list, vector, string, etc.).
If PRED is provided, take elements while PRED holds.

```elisp
(lazy-into-list (lazy-from-seq '(1 2 3)))
;;=> (1 2 3)

(lazy-into-list (lazy-from-seq [10 20 30]))
;;=> (10 20 30)

(lazy-into-list (lazy-from-seq "hello"))
;;=> (?h ?e ?l ?l ?o)

(lazy-into-list (lazy-from-seq '(1 2 3 4 5) (lambda (x) (< x 4))))
;;=> (1 2 3)
```

### From functions

#### `(lazy-repeat x)`

Create an infinite stream of X.

```elisp
(lazy-into-list (lazy-take 5 (lazy-repeat 42)))
;;=> (42 42 42 42 42)
```

#### `(lazy-repeatedly function)`

Create an infinite stream by calling FUNCTION repeatedly.

```elisp
(lazy-into-list (lazy-take 5 (lazy-repeatedly (lambda () (random 10)))))
;;=> (3 7 1 8 2)  ; random values
```

#### `(lazy-iterate function x)`

Create an infinite stream by applying FUNCTION to X repeatedly.

```elisp
(lazy-into-list (lazy-take 5 (lazy-iterate (lambda (x) (* x 2)) 1)))
;;=> (1 2 4 8 16)
```

#### `(lazy-unfold function seed)`

Return an infinite lazy sequence using FUNCTION and SEED.
FUNCTION takes a seed and returns (value . next-seed) or nil to stop.

```elisp
(lazy-into-list
 (lazy-unfold
  (lambda (n)
    (when (< n 5)
      (cons n (1+ n))))
  0))
;;=> (0 1 2 3 4)
```

### From random values

#### `(lazy-random &optional limit)`

Return an infinite lazy sequence of random numbers.
If LIMIT is provided, returns random integers in [0, LIMIT).
Otherwise returns random floats in [0.0, 1.0).

```elisp
(lazy-into-list (lazy-take 5 (lazy-random 100)))
;;=> (42 17 89 3 56)  ; random integers

(lazy-into-list (lazy-take 3 (lazy-random)))
;;=> (0.123456 0.789012 0.345678)  ; random floats
```

### From files

#### `(lazy-lines file)`

Return a lazy sequence of lines from FILE.

```elisp
(lazy-into-list (lazy-take 3 (lazy-lines "/etc/hosts")))
;;=> ("127.0.0.1 localhost" "::1 localhost" ...)
```

### From existing streams

#### `(lazy-cycle stream)`

Repeat STREAM infinitely.

```elisp
(lazy-into-list (lazy-take 10 (lazy-cycle (lazy-range 0 3))))
;;=> (0 1 2 0 1 2 0 1 2 0)
```

## Lazy-sequence in, Lazy-sequence out

Functions that transform lazy sequences into other lazy sequences.

### Taking and dropping elements

#### `(lazy-take n stream)`

Take the first N elements from STREAM.

```elisp
(lazy-into-list (lazy-take 3 (lazy-range)))
;;=> (0 1 2)
```

#### `(lazy-drop n stream)`

Drop the first N elements from STREAM and return the rest.

```elisp
(lazy-into-list (lazy-take 3 (lazy-drop 5 (lazy-range))))
;;=> (5 6 7)
```

#### `(lazy-take-while pred stream)`

Take elements from STREAM while PRED holds.

```elisp
(lazy-into-list (lazy-take-while (lambda (x) (< x 5)) (lazy-range)))
;;=> (0 1 2 3 4)
```

#### `(lazy-drop-while pred stream)`

Drop elements from STREAM while PRED holds.

```elisp
(lazy-into-list (lazy-take 3 (lazy-drop-while (lambda (x) (< x 5)) (lazy-range))))
;;=> (5 6 7)
```

#### `(lazy-butlast stream)`

Return STREAM without the last element.

```elisp
(lazy-into-list (lazy-butlast (lazy-range 0 5)))
;;=> (0 1 2 3)
```

#### `(lazy-drop-last n stream)`

Drop the last N elements from STREAM.

```elisp
(lazy-into-list (lazy-drop-last 2 (lazy-range 0 5)))
;;=> (0 1 2)
```

#### `(lazy-take-last n stream)`

Take the last N elements from STREAM.

```elisp
(lazy-into-list (lazy-take-last 2 (lazy-range 0 5)))
;;=> (3 4)
```

#### `(lazy-take-nth n stream)`

Take every Nth element from STREAM.

```elisp
(lazy-into-list (lazy-take-nth 2 (lazy-range 0 10)))
;;=> (0 2 4 6 8)
```

### Subsequences

#### `(lazy-subseq start end stream)`

Return a subsequence of STREAM from START to END.

```elisp
(lazy-into-list (lazy-subseq 5 10 (lazy-range)))
;;=> (5 6 7 8 9)
```

### Mapping

#### `(lazy-map function stream)`

Apply FUNCTION to each element of STREAM, returning a new stream.

```elisp
(lazy-into-list (lazy-take 5 (lazy-map (lambda (x) (* x x)) (lazy-range 1))))
;;=> (1 4 9 16 25)
```

#### `(lazy-mapcat function stream)`

Apply FUNCTION to each element of STREAM and concatenate results.

```elisp
(lazy-into-list
 (lazy-take 6
  (lazy-mapcat (lambda (x) (lazy-range x (+ x 2)))
               (lazy-range 0 3))))
;;=> (0 1 1 2 2 3)
```

#### `(lazy-mapn function &rest streams)`

Apply FUNCTION to elements from STREAMS in parallel.

```elisp
(lazy-into-list
 (lazy-take 3
  (lazy-mapn #'+
             (lazy-range 0)
             (lazy-range 10)
             (lazy-range 100))))
;;=> (110 113 116)
```

#### `(lazy-map-indexed function stream)`

Apply FUNCTION to index and each element of STREAM.

```elisp
(lazy-into-list
 (lazy-take 3
  (lazy-map-indexed (lambda (i x) (list i x))
                    (lazy-range 10 15))))
;;=> ((0 10) (1 11) (2 12))
```

### Filtering

#### `(lazy-filter pred stream)`

Filter STREAM to elements where PRED holds.

```elisp
(lazy-into-list
 (lazy-take 5
  (lazy-filter #'cl-evenp (lazy-range))))
;;=> (0 2 4 6 8)
```

#### `(lazy-remove pred stream)`

Remove elements from STREAM where PRED holds.

```elisp
(lazy-into-list
 (lazy-take 5
  (lazy-remove #'cl-evenp (lazy-range))))
;;=> (1 3 5 7 9)
```

#### `(lazy-keep function stream)`

Apply FUNCTION to elements of STREAM and keep non-nil results.

```elisp
(lazy-into-list
 (lazy-keep (lambda (x) (and (cl-evenp x) x))
            (lazy-range 0 10)))
;;=> (0 2 4 6 8)
```

### Removing duplicates

#### `(lazy-distinct stream)`

Remove duplicate elements from STREAM.

```elisp
(lazy-into-list
 (lazy-distinct
  (lazy-from-seq '(1 2 1 3 2 4))))
;;=> (1 2 3 4)
```

#### `(lazy-dedupe stream)`

Remove consecutive duplicate elements from STREAM.

```elisp
(lazy-into-list
 (lazy-dedupe
  (lazy-from-seq '(1 1 2 2 3 2 2))))
;;=> (1 2 3 2)
```

### Combining streams

#### `(lazy-append &rest streams)`

Append STREAMS into a single lazy sequence.

```elisp
(lazy-into-list
 (lazy-append (lazy-range 0 3)
              (lazy-range 10 13)))
;;=> (0 1 2 10 11 12)
```

#### `(lazy-concat streams)`

Concatenate a stream of STREAMS into a single stream.

```elisp
(lazy-into-list
 (lazy-take 6
  (lazy-concat
   (lazy-from-seq
    (list (lazy-range 0 3)
          (lazy-range 10 13))))))
;;=> (0 1 2 10 11 12)
```

#### `(lazy-interleave &rest streams)`

Interleave elements from STREAMS.

```elisp
(lazy-into-list
 (lazy-take 6
  (lazy-interleave (lazy-range 0 3)
                   (lazy-range 10 13))))
;;=> (0 10 1 11 2 12)
```

#### `(lazy-interpose separator stream)`

Insert SEPARATOR between elements of STREAM.

```elisp
(lazy-into-list
 (lazy-interpose :sep (lazy-range 0 3)))
;;=> (0 :sep 1 :sep 2)
```

#### `(lazy-flatten stream)`

Flatten one level of nesting in STREAM.

```elisp
(lazy-into-list
 (lazy-flatten
  (lazy-from-seq
   (list (lazy-range 0 3)
         (lazy-range 3 6)))))
;;=> (0 1 2 3 4 5)

(lazy-into-list
 (lazy-flatten
  (lazy-from-seq '(1 (2 3) 4))))
;;=> (1 2 3 4)
```

### Partitioning

#### `(lazy-partition n stream)`

Partition STREAM into chunks of size N.

```elisp
(lazy-into-list (lazy-partition 3 (lazy-range 0 10)))
;;=> ((0 1 2) (3 4 5) (6 7 8))
```

#### `(lazy-partition-by function stream)`

Partition STREAM when FUNCTION result changes.

```elisp
(lazy-into-list
 (lazy-partition-by (lambda (x) (< x 5))
                    (lazy-range 0 10)))
;;=> ((0 1 2 3 4) (5 6 7 8 9))
```

### Reduction with intermediate results

#### `(lazy-reductions function initial-value stream)`

Return a stream of successive reductions of STREAM.

```elisp
(lazy-into-list
 (lazy-reductions #'+ 0 (lazy-range 1 5)))
;;=> (0 1 3 6 10)
```

## Lazy-sequence in, value out

Functions that consume lazy sequences and return values.

### Accessing elements

#### `(lazy-elt n stream)`

Return the Nth element of STREAM (0-indexed).

```elisp
(lazy-elt 5 (lazy-range))
;;=> 5
```

#### `(lazy-second stream)`

Return the second element of STREAM.

```elisp
(lazy-second (lazy-range 10 20))
;;=> 11
```

#### `lazy-pop`

Pop and return the first element of STREAM, modifying STREAM.

```elisp
(setq s (lazy-range))
(lazy-pop s)  ; => 0
(lazy-pop s)  ; => 1
(lazy-pop s)  ; => 2
```

### Length

#### `(lazy-length stream)`

Return the length of STREAM.

```elisp
(lazy-length (lazy-range 0 10))
;;=> 10
```

#### `(lazy-bounded-length n stream)`

Return N if STREAM has at least N items, else the count of STREAM.
Safe for infinite streams.

```elisp
(lazy-bounded-length 10 (lazy-range 0 5))
;;=> 5

(lazy-bounded-length 5 (lazy-range))
;;=> 5  ; stops at 5 even for infinite stream
```

### Type checking

#### `(lazy-stream-p stream)`

Return t if STREAM is a lazy sequence.

```elisp
(lazy-stream-p (lazy-range))
;;=> t

(lazy-stream-p '(1 2 3))
;;=> nil
```

### Converting to lists

#### `(lazy-into-list stream)`

Convert STREAM into a list.

```elisp
(lazy-into-list (lazy-range 0 5))
;;=> (0 1 2 3 4)
```

### Side effects

#### `(lazy-do function stream)`

Apply FUNCTION to each element of STREAM for side effects.

```elisp
(lazy-do #'print (lazy-range 0 3))
;; Prints: 0 1 2
```

#### `lazy-dostream`

Iterate over STREAM, executing BODY for each element.

```elisp
(lazy-dostream (x (lazy-range 0 3))
  (message "Value: %s" x))
;; Prints: "Value: 0" "Value: 1" "Value: 2"
```

### Reduction

#### `(lazy-reduce function &rest args)`

Reduce STREAM using FUNCTION.

With 2 args: `(lazy-reduce function stream)`
With 3 args: `(lazy-reduce function initial-value stream)`

```elisp
(lazy-reduce #'+ (lazy-range 1 5))
;;=> 10

(lazy-reduce #'+ 0 (lazy-range 1 5))
;;=> 10

(lazy-reduce #'+ (lazy-from-seq '(42)))
;;=> 42
```

#### `(lazy-reduce-while pred function initial-value stream)`

Reduce STREAM while PRED holds.

```elisp
(lazy-reduce-while (lambda (acc x) (< acc 10))
                   #'+
                   0
                   (lazy-range 1))
;;=> 10
```

### Searching

#### `(lazy-find pred stream &optional default)`

Find the first element in STREAM where PRED holds.

```elisp
(lazy-find (lambda (x) (> x 5)) (lazy-range))
;;=> 6

(lazy-find #'stringp (lazy-range) 'not-found)
;;=> not-found
```

#### `(lazy-some pred stream)`

Return the first truthy result of PRED applied to STREAM elements.

```elisp
(lazy-some (lambda (x) (and (> x 5) x)) (lazy-range 0 10))
;;=> 6

(lazy-some (lambda (x) (> x 100)) (lazy-range 0 10))
;;=> nil
```

#### `(lazy-every pred stream)`

Return t if PRED holds for all elements of STREAM.

```elisp
(lazy-every (lambda (x) (>= x 0)) (lazy-range 0 10))
;;=> t

(lazy-every (lambda (x) (< x 5)) (lazy-range 0 10))
;;=> nil
```

### Splitting

#### `(lazy-split-at n stream)`

Split STREAM at position N, returning (BEFORE . AFTER).

```elisp
(let ((result (lazy-split-at 5 (lazy-range 0 10))))
  (list (lazy-into-list (car result))
        (lazy-into-list (cdr result))))
;;=> ((0 1 2 3 4) (5 6 7 8 9))
```

#### `(lazy-split-with pred stream)`

Split STREAM where PRED changes from true to false.

```elisp
(let ((result (lazy-split-with (lambda (x) (< x 5))
                                (lazy-range 0 10))))
  (list (lazy-into-list (car result))
        (lazy-into-list (cdr result))))
;;=> ((0 1 2 3 4) (5 6 7 8 9))
```

### Grouping

#### `(lazy-group-by function stream)`

Group elements of STREAM by the result of FUNCTION.
Return an alist of (key . list-of-elements).

```elisp
(lazy-group-by (lambda (x) (mod x 3))
               (lazy-range 0 10))
;;=> ((0 . (0 3 6 9)) (1 . (1 4 7)) (2 . (2 5 8)))
```

## Lowlevel functions

Low-level functions implementing SRFI 45 primitives. These are typically not used directly but form the foundation of the lazy evaluation system.

### Box operations

#### `(lazy-box x)`

Create a mutable box containing X.

#### `lazy-unbox`

Extract value from a box. Alias for `car`.

#### `lazy-setbox`

Set value in a box. Alias for `setcar`.

### Lazy evaluation primitives

#### `lazy`

Create a lazy promise that delays evaluation of BODY.

```elisp
(setq p (lazy (+ 1 2)))
(lazy-force p)  ; => 3
```

#### `lazy-delay`

Similar to `lazy` but for compatibility.

#### `(lazy-force promise)`

Force evaluation of a lazy PROMISE, memoizing the result.

### Stream construction

#### `lazy-cons`

Construct a lazy sequence with FIRST and REST.

```elisp
(setq s (lazy-cons 1 (lazy-cons 2 (lazy-nil))))
(lazy-car s)   ; => 1
(lazy-second s) ; => 2
```

#### `lazy-nil`, `lazy-null`

Empty stream and predicate to test for empty stream.

```elisp
(lazy-null (lazy-nil))  ; => t
(lazy-null (lazy-range)) ; => nil
```

#### `lazy-car`, `lazy-cdr`

Access head and tail of a lazy sequence.

```elisp
(lazy-car (lazy-range))  ; => 0
(lazy-into-list (lazy-take 3 (lazy-cdr (lazy-range))))
;;=> (1 2 3)
```

### Aliases

- `lazy-empty` → `lazy-nil`
- `lazy-empty-p` → `lazy-null`
- `lazy-first` → `lazy-car`
- `lazy-rest` → `lazy-cdr`

### CL-LOOP integration

lazy.el provides integration with `cl-loop` using the `lazy-by` keyword.

```elisp
(cl-loop repeat 10
         for i lazy-by (lazy-primes)
         collect i)
;;=> (2 3 5 7 11 13 17 19 23 29)
```

# Development

This project uses Nix flakes for reproducible development environments.

## Setup

Enable binary cache to avoid building Emacs from source:

```bash
nix-env -iA cachix -f https://cachix.org/api/v1/install
cachix use emacs-ci
```

Or add to your NixOS configuration: `https://emacs-ci.cachix.org`

## Usage

Enter the development shell (Emacs 30-1 by default):

```bash
nix develop
```

Run all tests across all Emacs versions:

```bash
nix flake check
```

Test with a specific Emacs version:

```bash
nix build -L .#checks.x86_64-linux.27-2
nix build -L .#checks.x86_64-linux.28-2
nix build -L .#checks.x86_64-linux.29-4
nix build -L .#checks.x86_64-linux.30-1
nix build -L .#checks.x86_64-linux.snapshot
```

Format Nix files:

```bash
nix fmt
```

# History

This library was originally written by chuntaro. Conao3 made some modifications to register it on MELPA. Thanks!

ref: https://github.com/chuntaro/lazy.el/issues/1
