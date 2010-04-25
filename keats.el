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

(defvar keats-mode-map (make-sparse-keymap)
  "Keymap for `keats-mode'.")

(define-prefix-command 'keats-mode-map)
(let ((map keats-mode-map))
  (define-key map (kbd "n") 'keats-new)
  map)


(defun keats-new ()
  "Adds a new keat."
  (interactive)
  (let ((keat (keats-read-keat)))
    (keats-create keat)))

(defun keats-create (keat)
  "Adds KEAT to the list of keats."
  (cond ((keats-valid-keat-p keat)
         (add-to-list 'keats-list keat t)
         (message "Successfully added keat for %s" (keats-keat-key keat)))
        (t (message "Keat is invalid and was not added"))))

(defun keats-read-keat ()
  "Reads a key binding and a description and returns a `keats-keat' struct object."
  (let ((cursor-in-echo-area t) (prompt "Key Binding: ") (key) (description) (res))
    ;; Read the key binding
    (setq key (keats-read-key prompt))
    (while (not (terminating-key-p key))
      (setq res (vconcat res key))
      (setq key (keats-read-key prompt (key-description res))))
    (when (string= (key-description key) "RET")
      ;; Read the description
      (setq description (read-string "Description: "))
      (make-keats-keat :key (key-description res) :description description))))

(defun keats-valid-keat-p (keat)
  "Returns t if KEAT is valid, nil otherwise."
  (and (keats-keat-p keat)
       (let ((key (keats-keat-key keat))
             (description (keats-keat-description keat)))
         (not (or (string= key "") (string= description ""))))))

(defun terminating-key-p (key)
  "Returns t if KEY is a terminating key, nil otherwise."
  (let ((description (key-description key)))
    (or (string= description "RET")
        (string= description "C-g"))))

(defun keats-read-key (&rest args)
  "Reads a key sequence and returns it as a vector."
  (let ((prompt (mapconcat 'identity args "")))
    (read-key-sequence-vector prompt)))

(define-minor-mode keats-mode
  "Keybinding Cheats."
  :init-value nil
  :lighter " Keats"
  :keymap keats-mode-map
  (let ((prefix (kbd "C-c k")))
    (if keats-mode
        (local-set-key prefix 'keats-mode-map)
      (local-unset-key prefix))))

(provide 'keats)

;;; keats.el ends here
