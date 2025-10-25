;;; nerd-icons-dired.el --- Shows icons for each file in dired mode -*- lexical-binding: t -*-

;; Copyright (C) 2023 Hongyu Ding <rainstormstudio@yahoo.com>

;; Author: Hongyu Ding <rainstormstudio@yahoo.com>
;; Keywords: lisp
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (nerd-icons "0.0.1"))
;; URL: https://github.com/rainstormstudio/nerd-icons-dired
;; Keywords: files, icons, dired

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

;; To use this package, simply install and add this to your init.el
;; (require 'nerd-icons-dired)
;; (add-hook 'dired-mode-hook 'nerd-icons-dired-mode)

;; or use use-package:
;; (use-package nerd-icons-dired
;;   :hook
;;   (dired-mode . nerd-icons-dired-mode))

;; This package is inspired by
;; - `all-the-icons-dired': https://github.com/jtbm37/all-the-icons-dired

;;; Code:

(require 'dired)
(require 'nerd-icons)

(defcustom nerd-icons-dired-infix-string "\t"
  "String inserted between the icon and the file name in the Dired buffer."
  :group 'nerd-icons
  :type 'string)

(defcustom nerd-icons-dired-special-prefix-string "  \t"
  "String inserted before file name of special entries in the Dired buffer."
  :group 'nerd-icons
  :type 'string)

(defcustom nerd-icons-dired-file-icon-function #'nerd-icons-icon-for-file
  "Function that get icon for file."
  :group 'nerd-icons
  :type 'function)

(defcustom nerd-icons-dired-dir-icon-function #'nerd-icons-icon-for-dir
  "Function that get icon for dir."
  :group 'nerd-icons
  :type 'function)

(defvar nerd-icons-dired-mode)

(defun nerd-icons-dired--add-overlay (pos string)
  "Add overlay to display STRING at POS."
  (let ((ov (make-overlay (1- pos) pos)))
    (overlay-put ov 'nerd-icons-dired-overlay t)
    (overlay-put ov 'after-string
                 ;; Workaround for the issue where overlapping faces
                 ;; are not applied
                 ;; https://github.com/rainstormstudio/nerd-icons-dired/issues/1
                 (propertize string 'display string))))

(defun nerd-icons-dired--overlays-in (beg end)
  "Get all nerd-icons-dired overlays between BEG to END."
  (cl-remove-if-not
   (lambda (ov)
     (overlay-get ov 'nerd-icons-dired-overlay))
   (overlays-in beg end)))

(defun nerd-icons-dired--overlays-at (pos)
  "Get nerd-icons-dired overlays at POS."
  (apply #'nerd-icons-dired--overlays-in `(,pos ,pos)))

(defun nerd-icons-dired--remove-all-overlays ()
  "Remove all `nerd-icons-dired' overlays."
  (save-restriction
    (widen)
    (mapc #'delete-overlay
          (nerd-icons-dired--overlays-in (point-min) (point-max)))))

(defun nerd-icons-dired--refresh ()
  "Display the icons of files in a Dired buffer."
  (nerd-icons-dired--remove-all-overlays)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (dired-move-to-filename nil)
        (let ((file (dired-get-filename 'relative 'noerror)))
          (when file
            (let ((icon (if (file-directory-p file)
                            (funcall nerd-icons-dired-dir-icon-function file)
                          (funcall nerd-icons-dired-file-icon-function file)))
                  (inhibit-read-only t))
              (if (member file '("." ".."))
                  (nerd-icons-dired--add-overlay (dired-move-to-filename)
                                                 nerd-icons-dired-special-prefix-string)
                (nerd-icons-dired--add-overlay (dired-move-to-filename)
                                               (concat icon nerd-icons-dired-infix-string)))))))
      (forward-line 1))))

(defun nerd-icons-dired--refresh-advice (&rest _ignored)
  "Refresh Dired icons when `nerd-icons-dired-mode' is enabled."
  (when nerd-icons-dired-mode
    (nerd-icons-dired--refresh)))

(defconst nerd-icons-dired--functions-to-refresh
  '( dired-readin dired-revert dired-internal-do-deletions dired-insert-subdir
     dired-create-directory dired-do-redisplay dired-kill-subdir dired-do-kill-lines
     dired-do-rename dired-narrow--internal dired-subtree-toggle dired-subtree-remove
     dired-subtree-cycle wdired-abort-changes)
  "List of Dired functions that need to refresh the icons." )

(defun nerd-icons-dired--add-advice ()
  "Add `nerd-icons-dired' advice."
  (dolist (fn nerd-icons-dired--functions-to-refresh)
    (advice-add fn :after #'nerd-icons-dired--refresh-advice)))

(defun nerd-icons-dired--remove-advice ()
  "Remove all `nerd-icons-dired' advice."
  (dolist (fn nerd-icons-dired--functions-to-refresh)
    (advice-remove fn #'nerd-icons-dired--refresh-advice)))

(defun nerd-icons-dired--setup ()
  "Setup `nerd-icons-dired'."
  (when (derived-mode-p 'dired-mode)
    (setq-local tab-width 1)
    (nerd-icons-dired--refresh)))

(defun nerd-icons-dired--teardown ()
  "Functions used as advice when redisplaying buffer."
  (nerd-icons-dired--remove-all-overlays))

;;;###autoload
(define-minor-mode nerd-icons-dired-mode
  "Display nerd-icons icon for each files in a Dired buffer."
  :lighter " nerd-icons-dired-mode"
  (when (derived-mode-p 'dired-mode)
    (if nerd-icons-dired-mode
        (nerd-icons-dired--setup)
      (nerd-icons-dired--teardown))))

;; Register advice on load and unregister on unload.
;; The advice itself will check if `nerd-icons-dired-mode'
;; is enabled.

(nerd-icons-dired--add-advice)

(defun nerd-icons-dired-unload-function ()
  "Unload function for `nerd-icons-dired'."
  (nerd-icons-dired--remove-advice)
  ;; continue with standard unloading
  nil)

(provide 'nerd-icons-dired)
;;; nerd-icons-dired.el ends here
