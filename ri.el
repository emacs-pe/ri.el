;;; ri.el --- Browse Ruby API documentation -*- lexical-binding: t -*-

;; Copyright © 2018 Javier Olaechea.

;; Author: Javier Olaechea <pirata@gmail.com>
;; URL: https://github.com/emacs-pe/ri.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: help ruby

;; This file is NOT part of GNU Emacs.

;; License

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ri integration

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'ansi-color)

(defgroup ri nil
  "Browse Ruby API documentation."
  :group 'ruby
  :group 'help
  :group 'external)

(defcustom ri-program-name "ri"
  "The path where the ri executable is located."
  :type 'string
  :group 'ri)

(defface ri-code
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey95")
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey5"))
  "Face for ri code blocks."
  :group 'ri)

(defvar ri-topics-history nil)

;; Based on `org-src-font-lock-fontify-block'
(defun ri-font-lock-fontify-block (mode start end)
  "Fontify code block with MODE from START to END."
  (let ((string (buffer-substring-no-properties start end))
        (buffer (current-buffer)))
    (remove-text-properties start end '(face nil))
    (with-current-buffer (get-buffer-create " *ri-fontification*")
      (let ((inhibit-modification-hooks nil))
        (erase-buffer)
        ;; add string and a final space to ensure property change.
        (insert string " "))
      (delay-mode-hooks (funcall mode))
      (font-lock-ensure)
      (let ((pos (point-min)) next)
        (while (setq next (next-property-change pos))
          ;; handle additional properties from font-lock, so as to
          ;; preserve, e.g., composition.
          (dolist (prop (cons 'face font-lock-extra-managed-props))
            (let ((new-prop (get-text-property pos prop)))
              (put-text-property (+ start (1- pos)) (1- (+ start next)) prop new-prop buffer)))
          (setq pos next))))))

(defun ri-fontify-code-regions ()
  "Capture code regions from buffer."
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (not (eobp))
             with (start end)
             if (looking-at "^  [^[:space:]]")
             do
             (setq start (match-beginning 0))
             (while (and (not (looking-at "^[^[:space:]]")) (not (eobp)))
               (forward-line)
               (setq end (1- (point))))
             (ri-font-lock-fontify-block 'ruby-mode start end)
             (font-lock-append-text-property start end 'face 'ri-code)
             else do (forward-line))))

(defun ri--output (topic)
  "Call ri command to fetch documentation about TOPIC."
  (with-temp-buffer
    (when (zerop (process-file ri-program-name nil t nil "--all" "--no-pager" "--format=ansi" topic))
      (let ((ansi-color-apply-face-function #'ansi-color-apply-text-property-face))
        (ansi-color-apply-on-region (point-min) (point-max)))
      (ri-fontify-code-regions)
      (buffer-string))))

(defun ri-thing-at-point ()
  "Return a symbol at point."
  (buffer-substring
   (save-excursion
     (while (or (not (zerop (skip-syntax-backward "w_.")))
                (not (zerop (skip-chars-backward ":")))))
     (point))
   (save-excursion
     (while (or (not (zerop (skip-syntax-forward "w_.")))
                (not (zerop (skip-chars-forward ":")))))
     (point))))

;;;###autoload
(defun ri (topic)
  "Browse Ruby API documentation about TOPIC."
  (interactive (list (completing-read "ri: " (process-lines ri-program-name "--list") nil nil (ri-thing-at-point) 'ri-topics-history)))
  (if-let ((documentation (ri--output topic)))
      (with-help-window "*ri*"
        (with-current-buffer standard-output
          (insert documentation)))
    (user-error "Nothing known about %s" topic)))

(provide 'ri)
;;; ri.el ends here
