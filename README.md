ivy
=====

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
