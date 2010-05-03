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
  "Prints keat description."
  (interactive)
  (keats-get
   (message (keats-keat-description keat))))

(defun keats-new ()
  "Adds new keat."
  (interactive)
  (let ((key (keats-read-key)))
    (when key
      (if (keats-find key)
          (message "Keat \"%s\" already defined" key)
        (keats-create key (keats-read-description))))))

(defun keats-create (key description)
  "Creates new keat and adds to list of keats."
  (let ((keat (make-keats-keat :key key :description description)))
    (add-to-list 'keats-list keat)
    (message "Successfully added keat \"%s\"" key)))

(defun keats-edit ()
  "Edits a keat description."
  (interactive)
  (keats-get
   (let* ((old-description (keats-keat-description keat))
          (new-description (keats-read-description old-description)))
     (keats-update keat new-description))))

(defun keats-update (keat description)
  "Updates KEAT's description."
  (setf (keats-keat-description keat) description))

(defun keats-destroy ()
  "Destroyes keat."
  (interactive)
  (keats-get
   (when (yes-or-no-p "Are you sure? ")
     (setq keats-list (delete* keat keats-list))
     (message "Successfully destroyed keat \"%s\"" key))))

(defmacro keats-get (&rest body)
  "Reads a key and yields if corresponding keat exists."
  `(let ((key (keats-read-key)) keat)
     (when key
       (setq keat (keats-find key))
       (if keat
           ,@body
         (message "No keat with key \"%s\" exists" key)))))

(defun keats-find (key)
  "Find keat with KEY."
  (let ((compare-fn
         (lambda (look-for keat)
           (string= look-for (keats-keat-key keat)))))
    (find key keats-list :test compare-fn)))

(defun keats-read-key ()
  "Reads a key binding."
  (let ((cursor-in-echo-area t) (prompt "Key Binding: ") key description res)
    (setq key (read-key-sequence-vector prompt))
    (while (not (keats-terminating-key-p key))
      (setq res (vconcat res key))
      (setq key (read-key-sequence-vector (concat prompt (key-description res)))))
    (if (string= (key-description key) "RET")
        (let ((key-binding (key-description res)))
          (if (keats-valid-key-p key-binding)
              key-binding
            (progn (message "Key \"%s\" is not a valid key" key-binding) nil))))))

(defun keats-terminating-key-p (key)
  (let ((description (key-description key)))
    (or (string= description "RET")
        (string= description "C-g"))))

(defun keats-valid-key-p (key)
  (and
   (not (equal key ""))
   (not (equal key nil))))

(defun keats-read-description (&optional initial-input)
  "Reads description."
  (read-string "Description: " initial-input))

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
    (add-to-list
     'keats-list
     (make-keats-keat
      :key (plist-get keat :key)
      :description (plist-get keat :description)))))

(defun keats-write ()
  "Writes all keats to `keats-file'."
  (interactive)
  (with-temp-file keats-file
    (insert (pp-to-string keats-list)))
  (message "Wrote keats to %s" keats-file))

;; Loads keats file.
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
