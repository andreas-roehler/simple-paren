##### Install
For instant use open simple-paren.el and M-x eval-buffer RET will provide it. 
After loading the file, M-x simple-paren-mode RET will turn the mode on, if it was off.
It will turn it off, if it was on.

Have a look at the key-setting, maybe edit the keymap in ‘simple-paren-mode-map’ at the end of the file.

In order to load simple-paren-mode from start, put the directory where the file resides into your Emacs path. For example:

(add-to-list 'load-path "PATH/TO/simple-paren.el") 
(require 'simple-paren)


