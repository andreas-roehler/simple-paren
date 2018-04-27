Simple Paren [![Build Status](https://travis-ci.org/andreas-roehler/simple-paren.svg?branch=master)](https://travis-ci.org/andreas-roehler/simple-paren)
===

###### Insert paired delimiters, wrap symbols in front maybe

Examples, cursor after pipe-symbol:

    (defun foo1 |       ==> (defun foo1 () 

    |interactive        ==> (interactive)

    int|eractive        ==> (interactive)

With active region until end of word

    int|eractive        ==> int(eractive)

With C-u insert literally:

    int|eractive        ==> (interactive)

keep padding:

    | foo               ==> ( foo ) 

###### Works also with reachable unary delimiters:

    |interactive      M-x simple-paren-colon RET  ==> :interactive:

###### Mathematicians resp. logicians might use

M-x simple-paren-ogham-feather-mark RET ==> ᚛᚜

or

M-x simple-paren-mathematical-double-angle-bracket RET ==> ⟪⟫

M-x simple-paren- TAB should display what's implemented.

###### Easy to extend:

If a command inserting mathematical white square brackets wouldn't exist:

    (defun simple-paren-mathematical-white-square-bracket (arg)
      (interactive "*P")
      (simple-paren--intern ?⟦  ?⟧ arg))

###### The mode provides keys like that:

    [(super \()] 'simple-paren-parentize

    [(super \/)] 'simple-paren-slash)

    [(super \<)] 'simple-paren-lesser-than)

Conceived for a deliberately non-electric use