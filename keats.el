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


(defvar keats-list ()
  "List containing all keats as `keats-keat' struct objects.")

(defvar keats-prefix-key "C-c k"
  "Prefix key for `keats-mode'.")

(defvar keats-mode-map (make-sparse-keymap)
  "Keymap for `keats-mode'.")


(defun keats-new ()
  "Adds a new keat."
  (interactive)
  (let ((keat (keats-read-keat)))
    (keats-create keat)))

(defun keats-edit ()
  "Edits an already existing keat."
  (interactive)
  (let ((key (keats-read-key)))
    (if (keats-exists-p key)
        (let ((description (keats-read-description))
              (keat (keats-find key)))
          (keats-update keat description))
      (message "No keat with key %s exists" key))))

(defun keats-create (keat)
  "Adds KEAT to the list of keats."
  (when keat
    (condition-case err
        (cond ((keats-valid-keat-p keat)
               (keats-add keat)
               (message "Successfully added keat for %s" (keats-keat-key keat)))
              (t
               (message "Keat is invalid and was not added")))
      (error
       (message (error-message-string err))))))

(defun keats-update (keat description)
  "Update KEAT's description."
  (setf (keats-keat-description keat) description))

(defun keats-add (keat)
  "Adds KEAT to the list of keats."
  (add-to-list 'keats-list keat t))

(defun keats-read-keat ()
  "Reads a key binding and a description and returns a `keats-keat' struct object."
  (let ((key (keats-read-key)))
    (when key
      (if (keats-exists-p key)
          (error "Keat for key %s already defined" key)
        (let ((description (keats-read-description)))
          (make-keats-keat :key key :description description))))))

(defun keats-read-key ()
  "Reads a keat key from the minibuffer."
  (let ((cursor-in-echo-area t) (prompt "Key Binding: ") key description res)
    (setq key (read-key-sequence-vector prompt))
    (while (not (keats-terminating-key-p key))
      (setq res (vconcat res key))
      (setq key (read-key-sequence-vector (concat prompt (key-description res)))))
    (if (string= (key-description key) "RET")
        (key-description res))))

(defun keats-read-description ()
  "Reads a keat description from the minibuffer."
  (read-string "Description: "))

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
             (define-key map (kbd "n") 'keats-new)
             (define-key map (kbd "e") 'keats-edit)))
          (t (local-unset-key prefix)))))


(provide 'keats)

;;; keats.el ends here
