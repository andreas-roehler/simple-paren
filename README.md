Simple Paren [![Build Status](https://travis-ci.org/emacs-berlin/simple-paren.svg?branch=master)](https://travis-ci.org/emacs-berlin/simple-paren)
===

# simple-paren
Insert paired delimiters, wrap symbols in front maybe

Examples, curor after pipe-symbol:

(defun foo1 | ==> (defun foo1 () 

|interactive ==> (interactive)

keeps padding
| foo == ( foo ) 
