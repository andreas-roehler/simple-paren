;;; simple-paren.el --- Insert paired delimiter, wrap symbols in front maybe  -*- lexical-binding: t; -*-

;; Version: 0.1
;; Copyright (C) 2016  Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@online.de>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Keywords: convenience

;;; Commentary: Provide common paired delimiters

;; Wrap symbols at point or at one space befor

;; Examples, curor as pipe-symbol:

;; (defun foo1 | ==> (defun foo1 ()

;; |interactive ==> (interactive)

;; (global-set-key [(super ?\()] 'simple-paren-parentize)
;; (global-set-key [(super ?{)] 'simple-paren-brace)
;; (global-set-key [(super ?\[)] 'simple-paren-bracket)
;; (global-set-key [(super ?')] 'simple-paren-singlequote)
;; (global-set-key [(super ?\")] 'simple-paren-doublequote)
;; (global-set-key [(super ?<)] 'simple-paren-lesser-then)
;; (global-set-key [(super ?>)] 'simple-paren-greater-then)

;; keeps padding
;; | foo == ( foo )
;;

;;; Code:

(defvar simple-paren-skip-chars "^\[\]{}(), \t\r\n\f"
  "Skip chars backward not mentioned here. ")

(defun simple-paren--return-complement-char-maybe (erg)
  "For example return \"}\" for \"{\" but keep \"\\\"\". "
  (pcase erg
    (?< ?>)
    (?> ?<)
    (?\( ?\))
    (?\) ?\()
    (?\] ?\[)
    (?\[ ?\])
    (?} ?{)
    (?{ ?})
    (_ erg)))

(defun simple-paren--intern (char)
  (let (end)
    (if (region-active-p)
	(progn
	  (setq end (copy-marker (region-end)))
	  (goto-char (region-beginning)))
      (skip-chars-backward simple-paren-skip-chars))
    (insert char)
    (if (region-active-p)
	(goto-char end)
      (when (looking-at "\\( \\)?[^ \n]+")
	;; travel symbols after point
	(skip-chars-forward " ") 
	(skip-chars-forward simple-paren-skip-chars)
	;; (forward-sexp)
	(when (match-string-no-properties 1)
	  (insert (match-string-no-properties 1))))))
  (insert (simple-paren--return-complement-char-maybe char))
  (forward-char -1))

;;;###autoload
(defun simple-paren-parentize ()
  (interactive "*")
  (simple-paren--intern ?\())

;;;###autoload
(defun simple-paren-bracket ()
  (interactive "*")
  (simple-paren--intern ?\[))

;;;###autoload
(defun simple-paren-brace ()
  (interactive "*")
  (simple-paren--intern ?{))

;;;###autoload
(defun simple-paren-doublequote ()
  (interactive "*")
  (simple-paren--intern ?\"))

;;;###autoload
(defun simple-paren-singlequote ()
  (interactive "*")
  (simple-paren--intern ?'))

;;;###autoload
(defun simple-paren-lesser-then ()
  (interactive "*")
  (simple-paren--intern ?<))

;;;###autoload
(defun simple-paren-greater-then ()
  (interactive "*")
  (simple-paren--intern ?>))

;;;###autoload
(defun simple-paren-grave-accent ()
  (interactive "*")
  (simple-paren--intern ?`))

;;;###autoload
(defun simple-paren-colon ()
  (interactive "*")
  (simple-paren--intern ?:))

;;;###autoload
(defun simple-paren-star ()
  (interactive "*")
  (simple-paren--intern ?*))

;;;###autoload
(defun simple-paren-equalize ()
  (interactive "*")
  (simple-paren--intern ?=))

;;;###autoload
(defun simple-paren-acute-accent ()
  (interactive "*")
  (simple-paren--intern ?´))



(provide 'simple-paren)
;;; simple-paren.el ends here
