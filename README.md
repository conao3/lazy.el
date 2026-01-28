# lazy.el
Lazy evaluation library for Emacs Lisp

Implementation of `SRFI 45' in Emacs Lisp and Lazy sequence functions.

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
;; Create an infinite sequence of natural numbers
(setq nums (lazy-range))

;; Take first 5 elements
(lazy-into-list (lazy-take 5 nums))
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

;; Combining multiple sequences
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

;; Infinite sequence
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

Create an infinite sequence of X.

```elisp
(lazy-into-list (lazy-take 5 (lazy-repeat 42)))
;;=> (42 42 42 42 42)
```

#### `(lazy-repeatedly function)`

Create an infinite sequence by calling FUNCTION repeatedly.

```elisp
(lazy-into-list (lazy-take 5 (lazy-repeatedly (lambda () (random 10)))))
;;=> (3 7 1 8 2)  ; random values
```

#### `(lazy-iterate function x)`

Create an infinite sequence by applying FUNCTION to X repeatedly.

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

### From existing sequences

#### `(lazy-cycle coll)`

Repeat COLL infinitely.

```elisp
(lazy-into-list (lazy-take 10 (lazy-cycle (lazy-range 0 3))))
;;=> (0 1 2 0 1 2 0 1 2 0)
```

## Lazy-sequence in, Lazy-sequence out

Functions that transform lazy sequences into other lazy sequences.

### Taking and dropping elements

#### `(lazy-take n coll)`

Take the first N elements from COLL.

```elisp
(lazy-into-list (lazy-take 3 (lazy-range)))
;;=> (0 1 2)
```

#### `(lazy-drop n coll)`

Drop the first N elements from COLL and return the rest.

```elisp
(lazy-into-list (lazy-take 3 (lazy-drop 5 (lazy-range))))
;;=> (5 6 7)
```

#### `(lazy-take-while pred coll)`

Take elements from COLL while PRED holds.

```elisp
(lazy-into-list (lazy-take-while (lambda (x) (< x 5)) (lazy-range)))
;;=> (0 1 2 3 4)
```

#### `(lazy-drop-while pred coll)`

Drop elements from COLL while PRED holds.

```elisp
(lazy-into-list (lazy-take 3 (lazy-drop-while (lambda (x) (< x 5)) (lazy-range))))
;;=> (5 6 7)
```

#### `(lazy-butlast coll)`

Return COLL without the last element.

```elisp
(lazy-into-list (lazy-butlast (lazy-range 0 5)))
;;=> (0 1 2 3)
```

#### `(lazy-drop-last n coll)`

Drop the last N elements from COLL.

```elisp
(lazy-into-list (lazy-drop-last 2 (lazy-range 0 5)))
;;=> (0 1 2)
```

#### `(lazy-take-last n coll)`

Take the last N elements from COLL.

```elisp
(lazy-into-list (lazy-take-last 2 (lazy-range 0 5)))
;;=> (3 4)
```

#### `(lazy-take-nth n coll)`

Take every Nth element from COLL.

```elisp
(lazy-into-list (lazy-take-nth 2 (lazy-range 0 10)))
;;=> (0 2 4 6 8)
```

### Subsequences

#### `(lazy-subseq start end coll)`

Return a subsequence of COLL from START to END.

```elisp
(lazy-into-list (lazy-subseq 5 10 (lazy-range)))
;;=> (5 6 7 8 9)
```

### Mapping

#### `(lazy-map function coll)`

Apply FUNCTION to each element of COLL, returning a new sequence.

```elisp
(lazy-into-list (lazy-take 5 (lazy-map (lambda (x) (* x x)) (lazy-range 1))))
;;=> (1 4 9 16 25)
```

#### `(lazy-mapcat function coll)`

Apply FUNCTION to each element of COLL and concatenate results.

```elisp
(lazy-into-list
 (lazy-take 6
  (lazy-mapcat (lambda (x) (lazy-range x (+ x 2)))
               (lazy-range 0 3))))
;;=> (0 1 1 2 2 3)
```

#### `(lazy-mapn function &rest colls)`

Apply FUNCTION to elements from COLLS in parallel.

```elisp
(lazy-into-list
 (lazy-take 3
  (lazy-mapn #'+
             (lazy-range 0)
             (lazy-range 10)
             (lazy-range 100))))
;;=> (110 113 116)
```

#### `(lazy-map-indexed function coll)`

Apply FUNCTION to index and each element of COLL.

```elisp
(lazy-into-list
 (lazy-take 3
  (lazy-map-indexed (lambda (i x) (list i x))
                    (lazy-range 10 15))))
;;=> ((0 10) (1 11) (2 12))
```

### Filtering

#### `(lazy-filter pred coll)`

Filter COLL to elements where PRED holds.

```elisp
(lazy-into-list
 (lazy-take 5
  (lazy-filter #'cl-evenp (lazy-range))))
;;=> (0 2 4 6 8)
```

#### `(lazy-remove pred coll)`

Remove elements from COLL where PRED holds.

```elisp
(lazy-into-list
 (lazy-take 5
  (lazy-remove #'cl-evenp (lazy-range))))
;;=> (1 3 5 7 9)
```

#### `(lazy-keep function coll)`

Apply FUNCTION to elements of COLL and keep non-nil results.

```elisp
(lazy-into-list
 (lazy-keep (lambda (x) (and (cl-evenp x) x))
            (lazy-range 0 10)))
;;=> (0 2 4 6 8)
```

### Removing duplicates

#### `(lazy-distinct coll)`

Remove duplicate elements from COLL.

```elisp
(lazy-into-list
 (lazy-distinct
  (lazy-from-seq '(1 2 1 3 2 4))))
;;=> (1 2 3 4)
```

#### `(lazy-dedupe coll)`

Remove consecutive duplicate elements from COLL.

```elisp
(lazy-into-list
 (lazy-dedupe
  (lazy-from-seq '(1 1 2 2 3 2 2))))
;;=> (1 2 3 2)
```

### Combining sequences

#### `(lazy-append &rest colls)`

Append COLLS into a single lazy sequence.

```elisp
(lazy-into-list
 (lazy-append (lazy-range 0 3)
              (lazy-range 10 13)))
;;=> (0 1 2 10 11 12)
```

#### `(lazy-concat colls)`

Concatenate a sequence of COLLS into a single sequence.

```elisp
(lazy-into-list
 (lazy-take 6
  (lazy-concat
   (lazy-from-seq
    (list (lazy-range 0 3)
          (lazy-range 10 13))))))
;;=> (0 1 2 10 11 12)
```

#### `(lazy-interleave &rest colls)`

Interleave elements from COLLS.

```elisp
(lazy-into-list
 (lazy-take 6
  (lazy-interleave (lazy-range 0 3)
                   (lazy-range 10 13))))
;;=> (0 10 1 11 2 12)
```

#### `(lazy-interpose separator coll)`

Insert SEPARATOR between elements of COLL.

```elisp
(lazy-into-list
 (lazy-interpose :sep (lazy-range 0 3)))
;;=> (0 :sep 1 :sep 2)
```

#### `(lazy-flatten coll)`

Flatten one level of nesting in COLL.

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

#### `(lazy-partition n coll)`

Partition COLL into chunks of size N.

```elisp
(lazy-into-list (lazy-partition 3 (lazy-range 0 10)))
;;=> ((0 1 2) (3 4 5) (6 7 8))
```

#### `(lazy-partition-by function coll)`

Partition COLL when FUNCTION result changes.

```elisp
(lazy-into-list
 (lazy-partition-by (lambda (x) (< x 5))
                    (lazy-range 0 10)))
;;=> ((0 1 2 3 4) (5 6 7 8 9))
```

### Reduction with intermediate results

#### `(lazy-reductions function initial-value coll)`

Return a sequence of successive reductions of COLL.

```elisp
(lazy-into-list
 (lazy-reductions #'+ 0 (lazy-range 1 5)))
;;=> (0 1 3 6 10)
```

## Lazy-sequence in, value out

Functions that consume lazy sequences and return values.

### Accessing elements

#### `(lazy-elt n coll)`

Return the Nth element of COLL (0-indexed).

```elisp
(lazy-elt 5 (lazy-range))
;;=> 5
```

#### `(lazy-second coll)`

Return the second element of COLL.

```elisp
(lazy-second (lazy-range 10 20))
;;=> 11
```

#### `lazy-pop`

Pop and return the first element of COLL, modifying COLL.

```elisp
(setq s (lazy-range))
(lazy-pop s)  ; => 0
(lazy-pop s)  ; => 1
(lazy-pop s)  ; => 2
```

### Length

#### `(lazy-length coll)`

Return the length of COLL.

```elisp
(lazy-length (lazy-range 0 10))
;;=> 10
```

#### `(lazy-bounded-length n coll)`

Return N if COLL has at least N items, else the count of COLL.
Safe for infinite sequences.

```elisp
(lazy-bounded-length 10 (lazy-range 0 5))
;;=> 5

(lazy-bounded-length 5 (lazy-range))
;;=> 5  ; stops at 5 even for infinite sequence
```

### Type checking

#### `(lazy-seq-p coll)`

Return t if COLL is a lazy sequence.

```elisp
(lazy-seq-p (lazy-range))
;;=> t

(lazy-seq-p '(1 2 3))
;;=> nil
```

### Converting to lists

#### `(lazy-into-list coll)`

Convert COLL into a list.

```elisp
(lazy-into-list (lazy-range 0 5))
;;=> (0 1 2 3 4)
```

### Side effects

#### `(lazy-do function coll)`

Apply FUNCTION to each element of COLL for side effects.

```elisp
(lazy-do #'print (lazy-range 0 3))
;; Prints: 0 1 2
```

#### `lazy-doseq`

Iterate over COLL, executing BODY for each element.

```elisp
(lazy-doseq (x (lazy-range 0 3))
  (message "Value: %s" x))
;; Prints: "Value: 0" "Value: 1" "Value: 2"
```

### Reduction

#### `(lazy-reduce function &rest args)`

Reduce COLL using FUNCTION.

With 2 args: `(lazy-reduce function coll)`
With 3 args: `(lazy-reduce function initial-value coll)`

```elisp
(lazy-reduce #'+ (lazy-range 1 5))
;;=> 10

(lazy-reduce #'+ 0 (lazy-range 1 5))
;;=> 10

(lazy-reduce #'+ (lazy-from-seq '(42)))
;;=> 42
```

#### `(lazy-reduce-while pred function initial-value coll)`

Reduce COLL while PRED holds.

```elisp
(lazy-reduce-while (lambda (acc x) (< acc 10))
                   #'+
                   0
                   (lazy-range 1))
;;=> 10
```

### Searching

#### `(lazy-find pred coll &optional default)`

Find the first element in COLL where PRED holds.

```elisp
(lazy-find (lambda (x) (> x 5)) (lazy-range))
;;=> 6

(lazy-find #'stringp (lazy-range) 'not-found)
;;=> not-found
```

#### `(lazy-some pred coll)`

Return the first truthy result of PRED applied to COLL elements.

```elisp
(lazy-some (lambda (x) (and (> x 5) x)) (lazy-range 0 10))
;;=> 6

(lazy-some (lambda (x) (> x 100)) (lazy-range 0 10))
;;=> nil
```

#### `(lazy-every pred coll)`

Return t if PRED holds for all elements of COLL.

```elisp
(lazy-every (lambda (x) (>= x 0)) (lazy-range 0 10))
;;=> t

(lazy-every (lambda (x) (< x 5)) (lazy-range 0 10))
;;=> nil
```

### Splitting

#### `(lazy-split-at n coll)`

Split COLL at position N, returning (BEFORE . AFTER).

```elisp
(let ((result (lazy-split-at 5 (lazy-range 0 10))))
  (list (lazy-into-list (car result))
        (lazy-into-list (cdr result))))
;;=> ((0 1 2 3 4) (5 6 7 8 9))
```

#### `(lazy-split-with pred coll)`

Split COLL where PRED changes from true to false.

```elisp
(let ((result (lazy-split-with (lambda (x) (< x 5))
                                (lazy-range 0 10))))
  (list (lazy-into-list (car result))
        (lazy-into-list (cdr result))))
;;=> ((0 1 2 3 4) (5 6 7 8 9))
```

### Grouping

#### `(lazy-group-by function coll)`

Group elements of COLL by the result of FUNCTION.
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

### Sequence construction

#### `lazy-cons`

Construct a lazy sequence with FIRST and REST.

```elisp
(setq s (lazy-cons 1 (lazy-cons 2 (lazy-nil))))
(lazy-car s)   ; => 1
(lazy-second s) ; => 2
```

#### `lazy-nil`, `lazy-null`

Empty sequence and predicate to test for empty sequence.

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
