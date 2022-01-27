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

A file can be formatted in place with:

```
$ raco fmt -i --width 80 <file.rkt>
```

Once installed, vim can be setup to run `raco fmt` automatically by adding the following to your vimrc:

```
" raco fmt on saving racket files
function! Racofmt()
  let l:save = winsaveview()
  %!raco fmt --width 80
  call winrestview(l:save)
endfun
autocmd BufWritePre *.rkt :call Racofmt()
```
