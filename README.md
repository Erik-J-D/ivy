ivy
===

# How to Run

TODO

# Tests

lol

# Tooling

## Auto-formatting
Currently using raco fmt:

```
$ raco pkg install fmt
```

All racket files in the repo can be formatted in place with:

```
$ find . -type f -name "*.rkt" | xargs raco fmt -i --width 80 --max-blank-lines 2
```
