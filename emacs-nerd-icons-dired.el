;;; emacs-nerd-icons-dired.el --- Shows icons for each file in dired mode -*- lexical-binding: t -*-

;; Copyright (C) 2023 Hongyu Ding <rainstormstudio@yahoo.com>

;; Author: Hongyu Ding <rainstormstudio@yahoo.com>
;; Keywords: lisp
;; Version: 0.0.1

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

;; uses emacs-nerd-icons for Dired

;;; Code:

(require 'dired)
(require 'emacs-nerd-icons)

(defface emacs-nerd-icons-dired-dir-face
  '((((background dark)) :foreground "white")
    (((background light)) :foreground "black"))
  "Face for the directory icon."
  :group 'emacs-nerd-icons-faces)

(defcustom emacs-nerd-icons-dired-v-adjust 0.01
  "The default vertical adjustment of the icon in the Dired buffer."
  :group 'emacs-nerd-icons
  :type 'number)

(defvar emacs-nerd-icons-dired-mode)

(defun emacs-nerd-icons-dired--add-overlay (pos string)
  "Add overlay to display STRING at POS."
  (let ((ov (make-overlay (1- pos) pos)))
    (overlay-put ov 'emacs-nerd-icons-dired-overlay t)
    (overlay-put ov 'after-string string)))

(defun emacs-nerd-icons-dired--overlays-in (beg end)
  "Get all emacs-nerd-icons-dired overlays between BEG to END."
  (cl-remove-if-not
   (lambda (ov)
     (overlay-get ov 'emacs-nerd-icons-dired-overlay))
   (overlays-in beg end)))

(defun emacs-nerd-icons-dired--overlays-at (pos)
  "Get emacs-nerd-icons-dired overlays at POS."
  (apply #'emacs-nerd-icons-dired--overlays-in `(,pos ,pos)))

(defun emacs-nerd-icons-dired--remove-all-overlays ()
  "Remove all `emacs-nerd-icons-dired' overlays."
  (save-restriction
    (widen)
    (mapc #'delete-overlay
          (emacs-nerd-icons-dired--overlays-in (point-min) (point-max)))))

(defun emacs-nerd-icons-dired--refresh ()
  "Display the icons of files in a Dired buffer."
  (emacs-nerd-icons-dired--remove-all-overlays)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (dired-move-to-filename nil)
        (let ((file (dired-get-filename 'relative 'noerror)))
          (when file
            (let ((icon (if (file-directory-p file)
                            (emacs-nerd-icons-icon-for-dir file
                                                        :face 'emacs-nerd-icons-dired-dir-face
                                                        :v-adjust emacs-nerd-icons-dired-v-adjust)
                          (emacs-nerd-icons-icon-for-file file :v-adjust emacs-nerd-icons-dired-v-adjust))))
              (if (member file '("." ".."))
                  (emacs-nerd-icons-dired--add-overlay (point) "  \t")
                (emacs-nerd-icons-dired--add-overlay (point) (concat icon "\t")))))))
      (forward-line 1))))

(defun emacs-nerd-icons-dired--refresh-advice (fn &rest args)
  "Advice function for FN with ARGS."
  (apply fn args)
  (when emacs-nerd-icons-dired-mode
    (emacs-nerd-icons-dired--refresh)))

(defun emacs-nerd-icons-dired--setup ()
  "Setup `emacs-nerd-icons-dired'."
  (when (derived-mode-p 'dired-mode)
    (setq-local tab-width 1)
    (advice-add 'dired-readin :around #'emacs-nerd-icons-dired--refresh-advice)
    (advice-add 'dired-revert :around #'emacs-nerd-icons-dired--refresh-advice)
    (advice-add 'dired-internal-do-deletions :around #'emacs-nerd-icons-dired--refresh-advice)
    (advice-add 'dired-insert-subdir :around #'emacs-nerd-icons-dired--refresh-advice)
    (advice-add 'dired-do-kill-lines :around #'emacs-nerd-icons-dired--refresh-advice)
    (with-eval-after-load 'dired-narrow
      (advice-add 'dired-narrow--internal :around #'emacs-nerd-icons-dired--refresh-advice))
    (emacs-nerd-icons-dired--refresh)))

(defun emacs-nerd-icons-dired--teardown ()
  "Functions used as advice when redisplaying buffer."
  (advice-remove 'dired-readin #'emacs-nerd-icons-dired--refresh-advice)
  (advice-remove 'dired-revert #'emacs-nerd-icons-dired--refresh-advice)
  (advice-remove 'dired-internal-do-deletions #'emacs-nerd-icons-dired--refresh-advice)
  (advice-remove 'dired-narrow--internal #'emacs-nerd-icons-dired--refresh-advice)
  (advice-remove 'dired-insert-subdir #'emacs-nerd-icons-dired--refresh-advice)
  (advice-remove 'dired-do-kill-lines #'emacs-nerd-icons-dired--refresh-advice)
  (emacs-nerd-icons-dired--remove-all-overlays))

;;;###autoload
(define-minor-mode emacs-nerd-icons-dired-mode
  "Display emacs-nerd-icons icon for each files in a Dired buffer."
  :lighter " emacs-nerd-icons-dired-mode"
  (when (derived-mode-p 'dired-mode)
    (if emacs-nerd-icons-dired-mode
        (emacs-nerd-icons-dired--setup)
      (emacs-nerd-icons-dired--teardown))))

(provide 'emacs-nerd-icons-dired)
;;; emacs-nerd-icons-dired.el ends here
