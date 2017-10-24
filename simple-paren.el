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

;; int|eractive ==> int(eractive)


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

(defvar simple-paren-braced-newline (list 'js-mode))

(defun simple-paren--intern (char &optional padding)
  (let (end)
    (if (region-active-p)
	(progn
	  (setq end (copy-marker (region-end)))
	  (goto-char (region-beginning)))
      (unless (or (eobp) (eolp)(member (char-after) (list 32 9)))
	(skip-chars-backward simple-paren-skip-chars)))
    (insert char)
    (if (region-active-p)
	(goto-char end)
      (when (and padding (looking-at "\\( \\)?[^ \n]+"))
	;; travel symbols after point
	(skip-chars-forward " "))
      (skip-chars-forward simple-paren-skip-chars)
      ;; (forward-sexp)
      (when (and padding (match-string-no-properties 1))
	(insert (match-string-no-properties 1))))
    (insert (simple-paren--return-complement-char-maybe char))
    (forward-char -1)
    (when (and (eq (char-after) ?})(member major-mode simple-paren-braced-newline))
      (newline 2)
      (indent-according-to-mode)
      (forward-char 1)
      (insert ?\;)
      (forward-line -1)
      (indent-according-to-mode))))

;;;###autoload
(defun simple-paren-parentize (&optional padding)
  "With \\[universal-argument] honor padding. "
  (interactive "*P")
  (simple-paren--intern ?\( (eq 4 (prefix-numeric-value padding))))

;;;###autoload
(defun simple-paren-bracket (&optional padding)
  "With \\[universal-argument] honor padding. "
  (interactive "*P")
  (simple-paren--intern ?\[ (eq 4 (prefix-numeric-value padding))))

;;;###autoload
(defun simple-paren-brace (&optional padding)
  "With \\[universal-argument] honor padding. "
  (interactive "*P")
  (simple-paren--intern ?{ (eq 4 (prefix-numeric-value padding))))

;;;###autoload
(defun simple-paren-doublequote (&optional padding)
  "With \\[universal-argument] honor padding. "
  (interactive "*P")
  (simple-paren--intern ?\" (eq 4 (prefix-numeric-value padding))))

;;;###autoload
(defun simple-paren-singlequote (&optional padding)
  "With \\[universal-argument] honor padding. "
  (interactive "*P")
  (simple-paren--intern ?' (eq 4 (prefix-numeric-value padding))))

(defun simple-paren-backtick (&optional padding)
  "With \\[universal-argument] honor padding. "
  (interactive "*P")
  (simple-paren--intern ?` (eq 4 (prefix-numeric-value padding))))

;;;###autoload
(defun simple-paren-lesser-then (&optional padding)
  "With \\[universal-argument] honor padding. "
  (interactive "*P")
  (simple-paren--intern ?< (eq 4 (prefix-numeric-value padding))))

;;;###autoload
(defun simple-paren-greater-then (&optional padding)
  "With \\[universal-argument] honor padding. "
  (interactive "*P")
  (simple-paren--intern ?> (eq 4 (prefix-numeric-value padding))))

;;;###autoload
(defun simple-paren-grave-accent (&optional padding)
  "With \\[universal-argument] honor padding. "
  (interactive "*P")
  (simple-paren--intern ?` (eq 4 (prefix-numeric-value padding))))

;;;###autoload
(defun simple-paren-colon (&optional padding)
  "With \\[universal-argument] honor padding. "
  (interactive "*P")
  (simple-paren--intern ?: (eq 4 (prefix-numeric-value padding))))

;;;###autoload
(defun simple-paren-star (&optional padding)
  "With \\[universal-argument] honor padding. "
  (interactive "*P")
  (simple-paren--intern ?* (eq 4 (prefix-numeric-value padding))))

;;;###autoload
(defun simple-paren-equalize (&optional padding)
  "With \\[universal-argument] honor padding. "
  (interactive "*P")
  (simple-paren--intern ?= (eq 4 (prefix-numeric-value padding))))

;;;###autoload
(defun simple-paren-acute-accent (&optional padding)
  "With \\[universal-argument] honor padding. "
  (interactive "*P")
  (simple-paren--intern ?´ (eq 4 (prefix-numeric-value padding))))



(provide 'simple-paren)
;;; simple-paren.el ends here
