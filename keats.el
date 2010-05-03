;;; keats.el --- Key binding cheats

;; Copyright (C) 2009-2010 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.1.0
;; Keywords: convenience, help
;; URL: http://github.com/rejeep/keats

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(defstruct keats-keat key description)

(add-hook 'kill-emacs-hook 'keats-write)


(defvar keats-list ()
  "List containing all keats as `keats-keat' struct objects.")

(defvar keats-file "~/.emacs.d/keats"
  "Path to file where keats are stored.")

(defvar keats-prefix-key "C-c k"
  "Prefix key for `keats-mode'.")

(defvar keats-mode-map (make-sparse-keymap)
  "Keymap for `keats-mode'.")


(defun keats-show ()
  "Print the description for input key."
  (interactive)
  (let ((key (keats-read-key)))
    (if (keats-exists-p key)
        (let ((keat (keats-find key)))
          (message (keats-keat-description keat)))
      (message "No keat with key %s exists" key))))

(defun keats-new ()
  "Adds a new keat."
  (interactive)
  (let* ((keat (keats-read-keat))
         (key (keats-keat-key keat)))
    (if (keats-exists-p key)
        (message "Keat for key %s already defined" (keats-keat-key keat))
      (keats-create keat))))

(defun keats-edit ()
  "Edits an already existing keat."
  (interactive)
  (let ((key (keats-read-key)))
    (if (keats-exists-p key)
        (let* ((keat (keats-find key))
               (old-description (keats-keat-description keat))
               (description (keats-read-description old-description)))
          (keats-update keat description))
      (message "No keat with key %s exists" key))))

(defun keats-destroy ()
  "Destroys an already existing keat."
  (interactive)
  (let ((key (keats-read-key)))
    (cond ((keats-exists-p key)
           (let ((keat (keats-find key)))
             (when (yes-or-no-p (concat "Remove keat " key "? "))
               (keats-remove keat)
               (message "Successfully destroyed keat for %s" key))))
          (t (message "No keat with key %s exists" key)))))

(defun keats-create (keat)
  "Adds KEAT to the list of keats."
  (cond ((and keat (keats-valid-keat-p keat))
         (keats-add keat)
         (message "Successfully added keat for %s" (keats-keat-key keat)))
        (t
         (message "Keat is invalid and was not added"))))

(defun keats-update (keat description)
  "Update KEAT's description."
  (setf (keats-keat-description keat) description))

(defun keats-add (keat)
  "Adds KEAT to the list of keats."
  (add-to-list 'keats-list keat t))

(defun keats-remove (keat)
  "Removes KEAT from the list of keats."
  ;; TODO: delete* should be destructive, but isn't. Why?
  (setq keats-list (delete* keat keats-list)))

(defun keats-read-keat ()
  "Reads a key binding and a description and returns a `keats-keat' struct object."
  (let ((key (keats-read-key)) description)
    (unless (keats-exists-p key)
      (setq description (keats-read-description)))
    (make-keats-keat :key key :description description)))

(defun keats-read-key ()
  "Reads a keat key from the minibuffer."
  (let ((cursor-in-echo-area t) (prompt "Key Binding: ") key description res)
    (setq key (read-key-sequence-vector prompt))
    (while (not (keats-terminating-key-p key))
      (setq res (vconcat res key))
      (setq key (read-key-sequence-vector (concat prompt (key-description res)))))
    (if (string= (key-description key) "RET")
        (key-description res))))

(defun keats-read-description (&optional initial-input)
  "Reads a keat description from the minibuffer."
  (read-string "Description: " initial-input))

(defun keats-valid-keat-p (keat)
  "Returns t if KEAT is valid, nil otherwise."
  (and (keats-keat-p keat)
       (let ((key (keats-keat-key keat))
             (description (keats-keat-description keat)))
         (not (or (string= key "") (string= description ""))))))

(defun keats-terminating-key-p (key)
  "Returns t if KEY is a terminating key, nil otherwise."
  (let ((description (key-description key)))
    (or (string= description "RET")
        (string= description "C-g"))))

(defun keats-exists-p (key)
  "Returns t if KEY is already defined, nil otherwise."
  (some (lambda (keat) (equal (keats-keat-key keat) key)) keats-list))

(defun keats-find (key)
  "Returns the keat with KEY if it exists, nil otherwise."
  (let ((compare-fn
         (lambda (look-for keat)
           (string= look-for (keats-keat-key keat)))))
    (find key keats-list :test compare-fn)))

(defun keats-load ()
  "Loads all from `keats-file' into `keats-list'."
  (if (file-exists-p keats-file)
      (let ((keats (keats-read)))
        (when keats
          (if (keats-keat-p (car keats))
              (setq keats-list keats)
            (keats-set-deprecated-format keats))))
    (write-file keats-file nil)))

(defun keats-read ()
  "Reads the contents of `keats-file' a list object representation."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents-literally keats-file)
        (read (current-buffer)))
    (error)))

(defun keats-set-deprecated-format (keats)
  "Sets `keats-list' to KEATS, which is a list of keats in the old format."
  (dolist (keat keats)
    (keats-add
     (make-keats-keat
      :key (plist-get keat :key)
      :description (plist-get keat :description)))))

(defun keats-write ()
  "Writes all keats to `keats-file'."
  (interactive)
  (with-temp-file keats-file
    (insert (pp-to-string keats-list))))

(keats-load)


;;;###autoload
(define-minor-mode keats-mode
  "Keybinding Cheats."
  :init-value nil
  :lighter " Keats"
  :keymap keats-mode-map
  (let ((prefix (read-kbd-macro keats-prefix-key)))
    (cond (keats-mode
           (define-prefix-command 'keats-mode-map)
           (local-set-key prefix 'keats-mode-map)
           (let ((map keats-mode-map))
             (define-key map (kbd "s") 'keats-show)
             (define-key map (kbd "n") 'keats-new)
             (define-key map (kbd "e") 'keats-edit)
             (define-key map (kbd "d") 'keats-destroy)
             (define-key map (kbd "w") 'keats-write)))
          (t (local-unset-key prefix)))))


(provide 'keats)

;;; keats.el ends here
