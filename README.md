Simple Paren [![Build Status](https://travis-ci.org/andreas-roehler/simple-paren.svg?branch=master)](https://travis-ci.org/andreas-roehler/simple-paren)
===

###### Insert paired delimiters, wrap symbols in front maybe

Examples, cursor after pipe-symbol:

(defun foo1 | ==> (defun foo1 () 

|interactive ==> (interactive)

With C-u keep padding:

| foo ==> ( foo ) 

###### Easy to extend:

In order to have a command inserting mathematical white square brackets, write:

`(defun simple-paren-mathematical-white-square-bracket (arg)
  (interactive "*P")
  (simple-paren--intern ?⟦  ?⟧ arg))`

###### Some examples how to set keys:

(global-set-key [(super \()] 'simple-paren-parentize)

(global-set-key [(super \/)] 'simple-paren-slash)

(global-set-key [(super \<)] 'simple-paren-lesser-then)

(global-set-key [(super \>)] 'simple-paren-greater-then)

(global-set-key [(super \[)] 'simple-paren-bracket)

(global-set-key [(super \\)] 'simple-paren-backslash)

(global-set-key [(super \{)] 'simple-paren-brace)


