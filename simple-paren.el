;;; simple-paren.el --- Insert paired delimiter, wrap -*- lexical-binding: t; -*-

;; Version: 0.1
;; Copyright (C) 2016-2017  Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@online.de>

;; URL: https://github.com/andreas-roehler/simple-paren

;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

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

;;; Commentary:
;; Commands inserting paired delimiters.  These are are easy to extend
;; by just providing the delimiting charakters as shown below with
;; math white square brackets

;; (defun simple-paren-mathematical-white-square-bracket (arg)
;;   "Insert MATHEMATICAL LEFT/RIGHT WHITE SQUARE BRACKETs

;; With \\[universal-argument] insert whitespaces literatim
;; With active region, wrap around.
;; With numerical ARG 2 honor padding"
;;   (interactive "*P")
;;   (simple-paren--intern ?⟦  ?⟧ arg))

;; Examples, cursor as pipe-symbol:

;; (defun foo1 |	==> (defun foo1 ()

;; |interactive		==> (interactive)

;; int|eractive		==> (interactive)

;; with active region until end of word
;; int|eractive		==> int(eractive)

;; With C-u keep padding
;; | foo		==> ( foo )

;; (global-set-key [(super ?\()] 'simple-paren-parentize)
;; (global-set-key [(super ?{)] 'simple-paren-brace)
;; (global-set-key [(super ?\[)] 'simple-paren-bracket)
;; (global-set-key [(super ?')] 'simple-paren-singlequote)
;; (global-set-key [(super ?\")] 'simple-paren-doublequote)
;; (global-set-key [(super ?<)] 'simple-paren-lesser-then)
;; (global-set-key [(super ?>)] 'simple-paren-greater-then)

;;

;;; Code:

(defvar simple-paren-skip-chars "^\[\]{}(), \t\r\n\f"
  "Skip chars backward not mentioned here.")

(defun simple-paren--return-complement-char-maybe (erg)
  "For example return \"}\" for \"{\" but keep \"\\\"\"."
  (pcase erg
    (?< ?>)
    (?> ?<)
    (?\( ?\))
    (?\) ?\()
    (?\] ?\[)
    (?\[ ?\])
    (?} ?{)
    (?{ ?})
    ;; '(leftrightsinglequote 8216 8217)
    ;; '(leftrightdoublequote 8220 8221)
    (8216 8217)
    (8220 8221)
    (_ erg)))

(defvar simple-paren-braced-newline (list 'js-mode))

(defun simple-paren--intern (left-char right-char &optional arg)
  (let ((padding (eq 2 (prefix-numeric-value arg)))
	(no-wrap (eq 4 (prefix-numeric-value arg)))
	end)
    (if no-wrap
	(progn
	  (insert left-char)
	  (insert right-char))
      (if (region-active-p)
	  (progn
	    (setq end (copy-marker (region-end)))
	    (goto-char (region-beginning)))
	(unless (or (eobp) (eolp)(member (char-after) (list 32 9)))
	  (skip-chars-backward simple-paren-skip-chars)))
      (insert left-char)
      (if (region-active-p)
	  (goto-char end)
	(when (and padding (looking-at "\\( \\)?[^ \n]+"))
	  ;; travel symbols after point
	  (skip-chars-forward " "))
	(skip-chars-forward simple-paren-skip-chars)
	;; (forward-sexp)
	(when (and padding (match-string-no-properties 1))
	  (insert (match-string-no-properties 1))))
      (insert right-char)
      (forward-char -1)
      (when (and (eq (char-after) ?})(member major-mode simple-paren-braced-newline))
	(newline 2)
	(indent-according-to-mode)
	(forward-char 1)
	(insert ?\;)
	(forward-line -1)
	(indent-according-to-mode)))))


;; ?⟦  ?⟧
(defun simple-paren-mathematical-left-white-square-bracket (arg)
  "(Insert MATHEMATICAL LEFT/RIGHT WHITE SQUARE BRACKETs."
  (interactive "*P")
  (simple-paren--intern ?⟦  ?⟧ arg)
  )

;; Commands
(defun simple-paren-brace (&optional arg)
  "With \\[universal-argument] insert braces literatim.

With active region, wrap around.
With numerical ARG 2 honor padding"
  (interactive "*P")
  (simple-paren--intern 123 125 arg))

(defun simple-paren-bracket (&optional arg)
  "With \\[universal-argument] insert brackets literatim.

With active region, wrap around.
With numerical ARG 2 honor padding"
  (interactive "*P")
  (simple-paren--intern 91 93 arg))

(defun simple-paren-lesser-than (&optional arg)
  "With \\[universal-argument] insert lesser-thans literatim.

With active region, wrap around.
With numerical ARG 2 honor padding"
  (interactive "*P")
  (simple-paren--intern 60 62 arg))

(defun simple-paren-greater-than (&optional arg)
  "With \\[universal-argument] insert greater-thans literatim.

With active region, wrap around.
With numerical ARG 2 honor padding"
  (interactive "*P")
  (simple-paren--intern 62 60 arg))

(defun simple-paren-leftrightsinglequote (&optional arg)
  "With \\[universal-argument] insert leftrightsinglequotes literatim.

With active region, wrap around.
With numerical ARG 2 honor padding"
  (interactive "*P")
  (simple-paren--intern 8216 8217 arg))

(defun simple-paren-leftrightdoublequote (&optional arg)
  "With \\[universal-argument] insert leftrightdoublequotes literatim.

With active region, wrap around.
With numerical ARG 2 honor padding"
  (interactive "*P")
  (simple-paren--intern 8220 8221 arg))

(defun simple-paren-parentize (&optional arg)
  "With \\[universal-argument] insert parentizes literatim.

With active region, wrap around.
With numerical ARG 2 honor padding"
  (interactive "*P")
  (simple-paren--intern 40 41 arg))

(defun simple-paren-acute-accent (&optional arg)
  "With \\[universal-argument] insert acute-accents literatim.

With active region, wrap around.
With numerical ARG 2 honor padding"
  (interactive "*P")
  (simple-paren--intern 180 180 arg))

(defun simple-paren-backslash (&optional arg)
  "With \\[universal-argument] insert backslashs literatim.

With active region, wrap around.
With numerical ARG 2 honor padding"
  (interactive "*P")
  (simple-paren--intern 92 92 arg))

(defun simple-paren-backtick (&optional arg)
  "With \\[universal-argument] insert backticks literatim.

With active region, wrap around.
With numerical ARG 2 honor padding"
  (interactive "*P")
  (simple-paren--intern 96 96 arg))

(defun simple-paren-colon (&optional arg)
  "With \\[universal-argument] insert colons literatim.

With active region, wrap around.
With numerical ARG 2 honor padding"
  (interactive "*P")
  (simple-paren--intern 58 58 arg))

(defun simple-paren-cross (&optional arg)
  "With \\[universal-argument] insert crosss literatim.

With active region, wrap around.
With numerical ARG 2 honor padding"
  (interactive "*P")
  (simple-paren--intern 43 43 arg))

(defun simple-paren-dollar (&optional arg)
  "With \\[universal-argument] insert dollars literatim.

With active region, wrap around.
With numerical ARG 2 honor padding"
  (interactive "*P")
  (simple-paren--intern 36 36 arg))

(defun simple-paren-doublequote (&optional arg)
  "With \\[universal-argument] insert doublequotes literatim.

With active region, wrap around.
With numerical ARG 2 honor padding"
  (interactive "*P")
  (simple-paren--intern 34 34 arg))

(defun simple-paren-equalize (&optional arg)
  "With \\[universal-argument] insert equalizes literatim.

With active region, wrap around.
With numerical ARG 2 honor padding"
  (interactive "*P")
  (simple-paren--intern 61 61 arg))

(defun simple-paren-escape (&optional arg)
  "With \\[universal-argument] insert escapes literatim.

With active region, wrap around.
With numerical ARG 2 honor padding"
  (interactive "*P")
  (simple-paren--intern 92 92 arg))

(defun simple-paren-grave-accent (&optional arg)
  "With \\[universal-argument] insert grave-accents literatim.

With active region, wrap around.
With numerical ARG 2 honor padding"
  (interactive "*P")
  (simple-paren--intern 96 96 arg))

(defun simple-paren-hash (&optional arg)
  "With \\[universal-argument] insert hashs literatim.

With active region, wrap around.
With numerical ARG 2 honor padding"
  (interactive "*P")
  (simple-paren--intern 35 35 arg))

(defun simple-paren-hyphen (&optional arg)
  "With \\[universal-argument] insert hyphens literatim.

With active region, wrap around.
With numerical ARG 2 honor padding"
  (interactive "*P")
  (simple-paren--intern 45 45 arg))

(defun simple-paren-singlequote (&optional arg)
  "With \\[universal-argument] insert singlequotes literatim.

With active region, wrap around.
With numerical ARG 2 honor padding"
  (interactive "*P")
  (simple-paren--intern 39 39 arg))

(defun simple-paren-slash (&optional arg)
  "With \\[universal-argument] insert slashs literatim.

With active region, wrap around.
With numerical ARG 2 honor padding"
  (interactive "*P")
  (simple-paren--intern 47 47 arg))

(defun simple-paren-star (&optional arg)
  "With \\[universal-argument] insert stars literatim.

With active region, wrap around.
With numerical ARG 2 honor padding"
  (interactive "*P")
  (simple-paren--intern 42 42 arg))

(defun simple-paren-tild (&optional arg)
  "With \\[universal-argument] insert tilds literatim.

With active region, wrap around.
With numerical ARG 2 honor padding"
  (interactive "*P")
  (simple-paren--intern 126 126 arg))

(defun simple-paren-underscore (&optional arg)
  "With \\[universal-argument] insert underscores literatim.

With active region, wrap around.
With numerical ARG 2 honor padding"
  (interactive "*P")
  (simple-paren--intern 95 95 arg))

(defun simple-paren-whitespace (&optional arg)
  "With \\[universal-argument] insert whitespaces literatim.

With active region, wrap around.
With numerical ARG 2 honor padding"
  (interactive "*P")
  (simple-paren--intern 32 32 arg))


(provide 'simple-paren)
;;; simple-paren.el ends here
