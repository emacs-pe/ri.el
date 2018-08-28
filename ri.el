;;; ri.el --- Browse Ruby API documentation -*- lexical-binding: t -*-

;; Copyright © 2018 Javier Olaechea.

;; Author: Javier Olaechea <pirata@gmail.com>
;; URL: https://github.com/emacs-pe/ri.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
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

;;; Code:

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

;; XXX: Should I create a buffer per class instead? e.j. *ri Array*?
(defcustom ri-process-buffer "*ri*"
  "The name of the buffer where ri outputs the documentation."
  :group 'ri)

(defun ri-all-known-topics ()
  "Return a list of all known topics of ri"
  (let* ((code "require 'rdoc/ri/driver'; \
                driver = RDoc::RI::Driver.new; \
                puts driver.list_known_classes; \
                puts driver.list_methods_matching('.');")
         (command (format "ruby -rrubygems -e\"%s\"" code)))
    (split-string (shell-command-to-string command))))


;;;###autoload
(defun ri (&optional ri-topic)
  (interactive)
  (let ((ri-topic (or ri-topic
                      (completing-read "ri: " (ri-all-known-topics) nil t (when-let (sym (symbol-at-point))
                                                                             (symbol-name sym))))))
    (with-current-buffer (get-buffer-create ri-process-buffer)
      (display-buffer (current-buffer))
      (erase-buffer)
      (insert (shell-command-to-string (format "%s --no-pager --format=ansi %s" ri-program-name (shell-quote-argument ri-topic))))
      (ansi-color-apply-on-region (point-min) (point-max))
      (beginning-of-buffer))))

(provide 'ri)
