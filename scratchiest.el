;;; scratchiest --- Make and manage scratch buffers. -*- lexical-binding: t; -*-

;; Copyright (C) 2025 precompute

;; Author: precompute <git@precompute.net>
;; URL: https://github.com/precompute/legit
;; Created: May 24, 2025
;; Modified: January 31, 2026
;; Version: 0.3.6
;; Package-Requires: ((emacs "26.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package helps create and manage *scratch* buffers.
;; - Backup and restore *scratch* buffers
;; - Create new *scratch* buffers
;; - Set default mode for *scratch* buffers
;; - Set custom scratch message
;; - Set custom suffix for buffer-name

;;; Code:
;;;; Variables
;;;;; Customizable
(defcustom scratchiest-message nil
  "Custom message for scratch buffers created by scratchiest.
Set to nil to use the value of `initial-scratch-message’."
  :type 'string :group 'scratchiest)

(defcustom scratchiest-suffix nil
  "Custom suffix for *scratch* buffers created by scratchiest."
  :type 'string :group 'scratchiest)

(defcustom scratchiest-always-inherit-major-mode nil
  "If non-nil, always inherit the major-mode of the buffer the command was called from."
  :type 'boolean :group 'scratchiest)

(defcustom scratchiest-backup-folder
  (concat (expand-file-name
           (file-name-as-directory user-emacs-directory)))
  "Backup folder for scratchiest backups."
  :type 'string :group 'scratchiest)

(defcustom scratchiest-always-new-window 'replace-scratch
  "If 'always, always open a new window when `scratchiest’ is called.
If 'never, always reuse the current window.
If 'replace-scratch, reuse the scratch window if it is the current
window.  Otherwise, open a new window."
  :type 'symbol :group 'scratchiest
  :options (list 'always 'replace-scratch 'never))

(defcustom scratchiest-suffix-generator 'timestamp
  "The kind of suffix generator to use for new scratch buffers."
  :type 'symbol :group 'scratchiest
  :options (list 'timestamp 'uuid))
;; :options (list 'number 'padded-number 'alphabets 'hex 'timestamp 'uuid))

(defcustom scratchiest-timestamp-format "%F %T.%3N"
  "Timestamp format used by `scratchiest-suffix-generator’.
See `format-time-string’ for format options."
  :type 'string :group 'scratchiest)

(defcustom scratchiest-arg-action 'copy-major-mode
  "Set the action for when `scratchiest’ is called with a prefix command.
When copy-major-mode is set, the value of `scratchiest-always-inherit-major-mode’ will be inverted.
When new-window is set, the value of `scratchiest-always-new-window’ will be inverted."
  :type 'symbol :group 'scratchiest
  :options (list 'copy-major-mode 'new-window))

;;;; Functions
(defun scratchiest--create-buffer (name)
  "Return a new buffer with the name NAME."
  (get-buffer-create name))

(defun scratchiest--in-scratch? ()
  "Return t if in a *scratch* buffer."
  (and (< 8 (length (buffer-name)))
       (string-equal "*scratch" (substring (buffer-name) 0 8))))

(defun scratchiest--generate-name ()
  "Generate the name for a new scratch buffer."
  (format "*scratch %s%s*"
          (cond
           ((eq scratchiest-suffix-generator 'timestamp)
            (format-time-string scratchiest-timestamp-format))
           ((eq scratchiest-suffix-generator 'uuid) ;; adapted from org-id-uuid
            (let ((rnd (md5 (format-time-string scratchiest-timestamp-format))))
              (format "%s-%s-4%s-%s%s-%s"
                      (substring rnd 0 8)
                      (substring rnd 8 12)
                      (substring rnd 13 16)
                      (format "%x"
                              (logior
                               #b10000000
                               (logand
                                #b10111111
                                (string-to-number
                                 (substring rnd 16 18) 16))))
                      (substring rnd 18 20)
                      (substring rnd 20 32)))))
          (or scratchiest-suffix "")))

(defun scratchiest (arg)
  "Make and/or switch to a scratch buffer.
ARG follows the choice in `scratchiest-arg-action’."
  (interactive "P")
  (let ((scratchiest-always-new-window
         (if (and arg (eq scratchiest-arg-action 'new-window))
             (if scratchiest-always-new-window nil 'always)
           scratchiest-always-new-window))
        (scratchiest-always-inherit-major-mode
         (if (and arg (eq scratchiest-arg-action 'copy-major-mode))
             (not scratchiest-always-inherit-major-mode)
           scratchiest-always-inherit-major-mode))
        (mmode major-mode)
        (buf (scratchiest--create-buffer
              (scratchiest--generate-name))))
    (message (format "mmode is %s, type %s" mmode (type-of mmode)))
    (with-current-buffer buf ;; stolen from get-scratch-buffer-create
      (when (or scratchiest-message initial-scratch-message)
        (insert (substitute-command-keys
                 (if scratchiest-message scratchiest-message initial-scratch-message)))
        (set-buffer-modified-p nil))
      (if scratchiest-always-inherit-major-mode
          (funcall mmode)
        (funcall initial-major-mode)))
    (cond ((eq scratchiest-always-new-window 'always)
           (pop-to-buffer buf))
          ((eq scratchiest-always-new-window 'never)
           (switch-to-buffer buf))
          ((eq scratchiest-always-new-window 'replace-scratch)
           (if (scratchiest--in-scratch?)
               (switch-to-buffer buf)
             (pop-to-buffer buf))))))

;;; provide
(provide 'scratchiest)

;;; TODO
;;;; Add functions for backup and restore
;;;; Functions to delete all scratch buffers other than this one
;;;; Unique ID generations that aren’t completely random
;;;; Make copied modes permanent (file variables?)
;;; scratchiest.el ends here
