# lazy.el
Lazy evaluation library for Emacs Lisp

Implementation of `SRFI 45' in Emacs Lisp and Stream functions.

SRFI 45  
https://srfi.schemers.org/srfi-45/srfi-45.html  
http://www.katch.ne.jp/~leque/translations/srfi-45/srfi-45j.html (Japanese)

It is used in the following article.  
https://qiita.com/chuntaro/items/f0d82f32cf216d4fd3dc (Japanese)

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

# TODO

## High Priority
- [x] lazy-concat - Concatenate a stream of streams
- [x] lazy-interleave - Interleave elements from multiple streams
- [x] lazy-cycle - Repeat a finite stream infinitely
- [x] lazy-repeat - Create an infinite stream of same value
- [x] lazy-repeatedly - Create an infinite stream by calling function repeatedly
- [x] lazy-iterate - Create an infinite stream by applying function repeatedly
- [x] lazy-distinct - Remove duplicate elements
- [x] lazy-dedupe - Remove consecutive duplicates
- [x] lazy-reductions - Return stream of successive reductions
- [x] lazy-split-at - Split stream at position N
- [x] lazy-split-with - Split stream where predicate changes
- [x] lazy-map-indexed - Map with index
- [x] lazy-take-nth - Take every Nth element
- [x] lazy-some - Find first truthy predicate result
- [x] lazy-every - Check if predicate holds for all elements
- [x] lazy-keep - Keep non-nil map results

## Medium Priority
- [x] lazy-partition - Partition stream into chunks
- [x] lazy-partition-by - Partition when function result changes
- [x] lazy-flatten - Flatten one level of nesting

# History

This library was originally written by chuntaro. Conao3 made some modifications to register it on MELPA. Thanks!

ref: https://github.com/chuntaro/lazy.el/issues/1
