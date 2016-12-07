;;; simple-paren-setup-ert-tests.el --- Provide needed forms -*- lexical-binding: t; -*-


;; Copyright (C) 2010-2016 Andreas Röhler, unless
;; indicated otherwise

;; Author: Andreas Röhler <andreas.roehler@easy-emacs.de>, unless
;; indicated otherwise

;; Keywords: lisp

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

;;; Commentary:

;;; Code:

(defvar simple-paren-install-directory default-directory)
(message "default-directory: %s" default-directory)

(sit-for 0.1 t)

(defvar simple-paren-debug-p nil
  "Avoid error")
;; (setq simple-paren-debug-p t)

(unless (featurep 'haskell) 
  (add-to-list 'load-path (concat simple-paren-install-directory ".cask/24.4/elpa/haskell-mode-20160818.437"))
  (if (file-readable-p
       (concat simple-paren-install-directory ".cask/24.4/elpa/haskell-mode-20160818.437/haskell.el"))
      (progn
	(message "Lade %s" (concat simple-paren-install-directory ".cask/24.4/elpa/haskell-mode-20160818.437/haskell.el"))
	(load (concat simple-paren-install-directory ".cask/24.4/elpa/haskell-mode-20160818.437/haskell.el") nil t))
    (message "Nicht gefunden: %s" (concat simple-paren-install-directory ".cask/24.4/elpa/haskell-mode-20160818.437/haskell.el"))))

;; .cask/24.4/elpa/php-mode-20160910.1801/
(unless (featurep 'php-mode)
  (add-to-list 'load-path (concat simple-paren-install-directory ".cask/24.4/elpa/php-mode-20160910.1801"))
  (if (file-readable-p
       (concat simple-paren-install-directory ".cask/24.4/elpa/php-mode-20160910.1801/php-mode.el"))
      (progn
	(message "Lade %s" (concat simple-paren-install-directory ".cask/24.4/elpa/php-mode-20160910.1801/php-mode.el"))
	(load (concat simple-paren-install-directory ".cask/24.4/elpa/php-mode-20160910.1801/php-mode.el") nil t))
    (message "Nicht gefunden: %s" (concat simple-paren-install-directory ".cask/24.4/elpa/php-mode-20160910.1801/php-mode.el"))))

(defmacro simple-paren-test (contents mode verbose &rest body)
  "Create temp buffer inserting CONTENTS.

BODY is code to be executed within the temp buffer "
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (insert ,contents)
       (funcall ,mode)
       (when ,verbose
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body))
  (sit-for 0.1))

(defmacro simple-paren-test-point-min (contents mode verbose &rest body)
  "Create temp buffer in `python-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (funcall ,mode)
       (insert ,contents)
       (goto-char (point-min))
       (when ,verbose
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body)))

(defmacro simple-paren-test-with-python-buffer (contents &rest body)
  "Create temp buffer in `python-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (and (featurep 'python) (unload-feature 'python))
     (let (hs-minor-mode)
       (insert ,contents)
       (python-mode)
       (when simple-paren-debug-p (switch-to-buffer (current-buffer))
	     (font-lock-fontify-buffer))
       ,@body))
  (sit-for 0.1))

(defmacro simple-paren-test-with-python-buffer-point-min (contents &rest body)
  "Create temp buffer in `python-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (and (featurep 'python) (unload-feature 'python))
     (let (hs-minor-mode)
       (insert ,contents)
       (python-mode)
       (goto-char (point-min))
       (when simple-paren-debug-p (switch-to-buffer (current-buffer))
	     (font-lock-fontify-buffer))
       ,@body)
     (sit-for 0.1)))

(defmacro simple-paren-test-with-php-buffer (contents &rest body)
  "Create temp buffer in `php-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (php-mode)
       (insert ,contents)
       (when simple-paren-debug-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body)))

(defmacro simple-paren-test-with-php-buffer-point-min (contents &rest body)
  "Create temp buffer in `php-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (php-mode)
       (insert ,contents)
       (goto-char (point-min))
       (when simple-paren-debug-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body)))

(defmacro simple-paren-test-with-ruby-buffer (contents &rest body)
  "Create temp buffer in `ruby-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (ruby-mode)
       (insert ,contents)
       (when simple-paren-debug-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body)))

(defmacro simple-paren-test-with-ruby-buffer-point-min (contents &rest body)
  "Create temp buffer in `php-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (ruby-mode)
       (insert ,contents)
       (goto-char (point-min))
       (when simple-paren-debug-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body)))

(defmacro simple-paren-test-with-elisp-buffer (contents &rest body)
  "Create temp buffer in `emacs-lisp-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (switch-to-buffer (current-buffer))
     ;; (and (featurep 'python) (unload-feature 'python))
     (let (hs-minor-mode)
       (emacs-lisp-mode)
       (insert ,contents)
       (when simple-paren-debug-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body)))

(defmacro simple-paren-test-with-elisp-buffer-point-min (contents &rest body)
  "Create temp buffer in `emacs-lisp-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (switch-to-buffer (current-buffer))
     ;; (and (featurep 'python) (unload-feature 'python))
     (let (hs-minor-mode)
       (emacs-lisp-mode)
       (insert ,contents)
       (goto-char (point-min)) 
       (when simple-paren-debug-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body)))

(defmacro simple-paren-test-with-js-buffer-point-min (contents &rest body)
  "Create temp buffer in `js-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
;;     (and (featurep 'js) (unload-feature 'js))
     (let (hs-minor-mode)
       (js-mode)
       (insert ,contents)
       (goto-char (point-min))
       (when simple-paren-debug-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body)))

(defmacro simple-paren-test-with-js-buffer (contents &rest body)
  "Create temp buffer in `js-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (js-mode)
       (insert ,contents)
       (when simple-paren-debug-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body)))

(defmacro simple-paren-test-with-temp-buffer (contents &rest body)
  "Create temp buffer inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (insert ,contents)
       (when simple-paren-debug-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body)))

(defmacro simple-paren-test-with-temp-buffer-point-min (contents &rest body)
  "Create temp buffer inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (insert ,contents)
       (goto-char (point-min))
       (when simple-paren-debug-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body)))

(defmacro simple-paren-test-with-nxml-buffer (contents &rest body)
  "Create temp buffer in `nxml-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     ;; (and (featurep 'python) (unload-feature 'python))
     (let (hs-minor-mode)
       (nxml-mode)
       (insert ,contents)
       (when simple-paren-debug-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body)))

(defmacro simple-paren-test-with-html-buffer (contents &rest body)
  "Create temp buffer in `nxml-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     ;; (and (featurep 'python) (unload-feature 'python))
     (let (hs-minor-mode)
       (html-mode)
       (insert ,contents)
       (when simple-paren-debug-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body)))

(defmacro simple-paren-test-with-haskell-buffer (contents &rest body)
  "Create temp buffer in `haskell-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (insert ,contents)
       (haskell-mode)
       (when simple-paren-debug-p (switch-to-buffer (current-buffer))
	     (font-lock-fontify-buffer))
       ,@body)
     (sit-for 0.1)))

(defmacro simple-paren-test-with-haskell-buffer-point-min (contents &rest body)
  "Create temp buffer in `haskell-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (insert ,contents)
       (haskell-mode)
       ;; (message "fill-paragraph-function: %s" fill-paragraph-function)
       (goto-char (point-min))
       (when simple-paren-debug-p (switch-to-buffer (current-buffer))
	     (font-lock-fontify-buffer))
       ,@body)
     (sit-for 0.1)))


(defmacro simple-paren-test-with-shell-script-buffer (contents &rest body)
  "Create temp buffer inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (insert ,contents)
       (shell-script-mode)
       (when simple-paren-debug-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))

       ,@body)))

(defmacro simple-paren-test-with-shell-script-buffer-point-min (contents &rest body)
  "Create temp buffer inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (insert ,contents)
       (shell-script-mode)
       (goto-char (point-min))
       (when simple-paren-debug-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body)))

(provide 'simple-paren-setup-ert-tests)
;; simple-paren-setup-ert-tests.el ends here
