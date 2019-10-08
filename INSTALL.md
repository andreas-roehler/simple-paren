##### Install

In order to load simple-paren-mode from start, put the directory where the file resides into your Emacs path. For example:

(add-to-list 'load-path "PATH/TO/simple-paren.el") 
(require 'simple-paren)

After loading the file, M-x simple-paren-mode RET will turn the mode on, if it was off.

It will turn it off, it it was on.

Maybe edit the keymap in ‘simple-paren-mode-map’ at the end of the file at your convenience.

