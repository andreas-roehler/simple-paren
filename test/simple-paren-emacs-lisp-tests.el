;;; simple-paren-emacs-lisp-tests.el ---  Tests -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2016 Andreas Röhler, unless
;; indicated otherwise

;; Author: Andreas Röhler <andreas.roehler@easy-emacs.de>, unless
;; indicated otherwise

;; Keywords: languages, lisp

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

;;; Commentary: A still naive implementation of a simple-paren command

;;

;;; Code:

(ert-deftest simple-paren--elisp-parentize-test-1 ()
  (simple-paren-test-with-elisp-buffer-point-min
    "asdf"
    (simple-paren-parentize)
    (should (eq (char-after) ?\)))))

(ert-deftest simple-paren--elisp-parentize-test-2 ()
  (simple-paren-test-with-elisp-buffer
    "asdf"
      (forward-char -1)
    (simple-paren-parentize)
    (goto-char (point-min))
    (should (eq (char-after) ?\())))

(ert-deftest simple-paren--elisp-colon-test-1 ()
  (simple-paren-test-with-elisp-buffer
    "asdf"
      (forward-char -1)
    (simple-paren-colon)
    (goto-char (point-min))
    (should (eq (char-after) ?:))))

(ert-deftest simple-paren--elisp-singlequote-test-1 ()
  (simple-paren-test-with-elisp-buffer
      "print(asdf)"
      (forward-char -5)
    (simple-paren-singlequote)
    (should (eq (char-after) ?'))))

(ert-deftest simple-paren--elisp-doublequote-test-1 ()
  (simple-paren-test-with-elisp-buffer
      " foo "
      (forward-char -4)
    (simple-paren-doublequote)
    (should (eq (char-after) ?\"))))

(ert-deftest simple-paren--elisp-doublequote-test-2 ()
  (simple-paren-test-with-elisp-buffer-point-min
      " foo "
      (simple-paren-doublequote 2)
    (should (eq (char-after) ?\"))
    (should (eq (char-before) 32))))

(ert-deftest simple-paren--elisp-paren-test-1 ()
  (simple-paren-test-with-elisp-buffer
      "()"
      (forward-char -1)
    (simple-paren-parentize)
    (goto-char (point-max))
    (should (eq (char-before) ?\)))
    (should (eq 5 (point)))))

(ert-deftest simple-paren--elisp-paren-test-2 ()
  (simple-paren-test-with-elisp-buffer-point-min
      "(asdf)"
    (simple-paren-parentize)
    (goto-char (point-max))
    (should (eq (char-before) ?\)))
    (should (eq 9 (point)))))

(ert-deftest simple-paren--elisp-paren-test-3 ()
  (simple-paren-test-with-elisp-buffer
      "{}"
      (forward-char -1)
    (simple-paren-parentize)
    (should (eq (char-after) ?\)))
    (should (eq 3 (point)))))

(ert-deftest simple-paren--elisp-paren-test-4 ()
  (simple-paren-test-with-elisp-buffer
      "{asdf}"
      (forward-char -1)
    (simple-paren-parentize)
    (should (eq (char-after) ?\)))
    (should (eq 7 (point)))))


(ert-deftest simple-paren--elisp-paren-test-5 ()
  (simple-paren-test-with-elisp-buffer
      "[]"
      (forward-char -1)
    (simple-paren-parentize)
    (should (eq (char-after) ?\)))
    (should (eq 3 (point)))))

(ert-deftest simple-paren--elisp-paren-test-6 ()
  (simple-paren-test-with-elisp-buffer
      "[asdf]"
      (forward-char -1)
    (simple-paren-parentize)
    (should (eq (char-after) ?\)))
    (should (eq 7 (point)))))

(ert-deftest simple-paren--elisp-paren-test-7 ()
  (simple-paren-test-with-elisp-buffer
      "[asdf]"
      (push-mark nil nil t)
    (beginning-of-line)
    (simple-paren-ogham-feather-mark)
    (should (eq (char-after) ?᚜))
    ;; (should (eq 8 (point)))
    ))


(ert-deftest simple-paren--elisp-paren-test-8 ()
  (simple-paren-test-with-elisp-buffer
      "[asdf]"
      (forward-char -1)
      (simple-paren-ogham-feather-mark)
    (goto-char (point-max))
    (forward-char -1)
    (should (eq (char-before) ?᚜))
    (should (eq 8 (point)))))

(ert-deftest simple-paren--python-singlequote-test-1 ()
  (simple-paren-test-with-python-buffer
      "a = [asdf]"
      (forward-char -5)
    (simple-paren-singlequote)
    (should (eq (char-after) ?'))))

(ert-deftest simple-paren--python-eol-test-1 ()
  (simple-paren-test-with-python-buffer
      "def foo"
    (simple-paren-parentize)
    (forward-char 1)
    (should (eq (char-before) ?\())))

(provide 'simple-paren-emacs-lisp-tests)
;;; simple-paren-emacs-lisp-tests.el ends here
