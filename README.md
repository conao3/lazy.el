# lazy.el
Lazy evaluation library for Emacs Lisp

Implementation of `SRFI 45' in Emacs Lisp and Stream functions.

SRFI 45  
https://srfi.schemers.org/srfi-45/srfi-45.html  
http://www.katch.ne.jp/~leque/translations/srfi-45/srfi-45j.html (Japanese)

It is used in the following article.  
https://qiita.com/chuntaro/items/f0d82f32cf216d4fd3dc (Japanese)

# Development

## Update nixpkgs hash

To update the nixpkgs hash in `shell.nix`:

```bash
COMMIT=$(curl -sL https://api.github.com/repos/NixOS/nixpkgs/commits/nixpkgs-unstable | grep -m1 '"sha"' | cut -d'"' -f4)
SHA256=$(nix-prefetch-url --unpack https://github.com/NixOS/nixpkgs/archive/${COMMIT}.tar.gz 2>&1 | tail -1)
cat <<EOF
  nixpkgs = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${COMMIT}.tar.gz";
    sha256 = "${SHA256}";
  };
EOF
```

Copy the output and replace the `nixpkgs = fetchTarball { ... };` section in `shell.nix`.

# History

This library was originally written by chuntaro. Conao3 made some modifications to register it on MELPA. Thanks!

ref: https://github.com/chuntaro/lazy.el/issues/1
