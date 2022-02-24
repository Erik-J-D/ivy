ivy
=====

Ivy aims to be a text editor (very) loosely inspired by vim and emacs, for the 21st century. It is in very early development and could be more accurately thought of as a collection of hopes and dreams, than a working editor.

## Design Goals / Decisions

- Written in Racket. This was chosen because Racket is:
    - reasonably performant
    - well supported across OSes
    - interpreted, this makes it easier to write plugins (this is step Rust failed)
- Backed by a rope
- Offload *all* language processing onto LSP servers and treesitter.
- Write a backend and frontend independently so you can swap out frontends
    - start off with a vim-inspired CLI frontend

## Inspiration
- [Xi Editor](https://github.com/xi-editor/xi-editor)
    - I hadn't actually seen it when putting together a napkin-sketch plan for ivy, interesting to see we arrived at many similar design decisions

## How to Run

TODO

## Tests

lol

## Tooling

### Auto-formatting
Currently using raco fmt:

```
$ raco pkg install fmt
```

All racket files in the repo can be formatted in place with:

```
$ find . -type f -name "*.rkt" | xargs raco fmt -i --width 80 --max-blank-lines 2
```

A precommit-hook can be added by adding the following to `.git/hooks/pre-commit` and making it executable:

```
#!/usr/bin/env bash

FILES=$(git diff --name-only --cached --diff-filter=ACMR | grep ".*\.rkt$" | sed 's| |\\ |g')
[ -z "$FILES" ] && exit 0

# Format files
 echo "$FILES" | xargs raco fmt -i --width 80 --max-blank-lines 2

 # Add files back
 echo "$FILES" | xargs git add

 exit 0
```
