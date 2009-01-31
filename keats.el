;;; keats.el --- Keyboard shortcut cheats

;; Copyright 2008  Johan Andersson

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; License ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Vocabulary ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;; Description ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;; Installation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; To use this mode you first have to make sure that this file is in
;; your load-path variable:
;; (add-to-list 'load-path "/path/to/directory/or/file")
;;
;; Then require it:
;; (require 'keats)
;;
;; Then start it:
;; (keats-mode t) or M-x keats-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Commentary ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; History ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst keats-version ""
  "Keats version.")

(defvar keats-mode-map (make-sparse-keymap)
  "Keymap for `keats-mode'.")

(define-prefix-command 'keats-mode-map)
(global-set-key (kbd "C-c k") 'keats-mode-map)
(let ((map keats-mode-map))
  (define-key map (kbd "a") 'keats-add)
  (define-key map (kbd "e") 'keats-edit)
  (define-key map (kbd "r") 'keats-remove)
  (define-key map (kbd "d") 'keats-print-description)
  (define-key map (kbd "s") 'keats-search))

(defvar keats-file "~/.keats"
  "Path to file where keats are stored.")

(defvar keats-temp-buffer "*keats*"
  "Temp buffer.")

(defvar keats-delimiter "|"
  "The delimiter used in `keats-file' to separate key sequence
  from description. Don't change this when you already have
  contents in you `keats-file'. If you do this, old keats will
  not be recognized.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun keats-add (&optional key description)
  "Adds a keat to `keats-file' if it does not already exist. If
it exists, `keats-edit' is called if user confirms."
  (interactive)
  (setq key (keats-key-or-read-key key))
  (cond ((and key (keats-find-key-position key))
         (if (yes-or-no-p "Already exists. Do you want to edit it? ")
             (keats-edit key)))
        (t
         (or description (setq description (read-string "Description: ")))
         (find-file keats-file)
         (goto-char (point-max))
         (unless (= (current-column) 0)
           (let ((next-line-add-newlines t))
             (next-line)))
         (insert (concat key keats-delimiter description))
         (save-buffer)
         (kill-this-buffer)
         (print (concat key " added")))))

(defun keats-edit (&optional key description)
  "Edits a keat in `keats-file' if it exists."
  (interactive)
  (setq key (keats-key-or-read-key key))
  (let ((line) (pos (keats-find-key-position key)))
    (cond ((and key pos)
           (or description (setq description (read-string "Description: " (keats-get-description key))))
           (find-file keats-file)
           (goto-char pos)
           (replace-regexp (concat keats-delimiter ".*") (concat keats-delimiter description) nil (line-beginning-position) (line-end-position))
           (save-buffer)
           (kill-this-buffer)
           (print (concat "Updated " key)))
          (t
           (print (concat key " not found"))))))

(defun keats-remove (&optional key)
  "Removes the given key sequence from `keats-file'."
  (interactive)
  (setq key (keats-key-or-read-key key))
  (let ((pos (keats-find-key-position key)))
    (cond ((and key pos)
           (find-file keats-file)
           (goto-char pos)
           (set-mark (point))
           (let ((next-line-add-newlines t))
             (next-line))
           (delete-region (point) (mark))
           (save-buffer)
           (kill-this-buffer)
           (print (concat key " removed")))
          (t
           (print (concat key " not found"))))))

(defun keats-get-description (&optional key)
  "Returns the description of the given key sequence."
  (setq key (keats-key-or-read-key key))
  (let ((res)
        (string)
        (pos (keats-find-key-position key)))
    (cond ((and key pos)
           (find-file keats-file)
           (goto-char pos)
           (setq string (buffer-substring (line-beginning-position) (line-end-position)))
           (string-match (concat "^" key keats-delimiter "\\(.*\\)$") string)
           (setq res (match-string-no-properties 1 string))
           (kill-this-buffer)
           res))))

(defun keats-print-description (&optional key)
  "Prints the description of the given key sequence."
  (interactive)
  (setq key (keats-key-or-read-key key))
  (let ((res (keats-get-description key)))
    (if res
        (print res)
      (print (concat key " not found")))))

(defun keats-search (query)
  "Searches in `keats-file' for lines that matches QUERY as
description."
  (interactive "*sQuery: ")
  (let ((res) (case-fold-search t))
    (switch-to-buffer (get-buffer-create keats-temp-buffer))
    (delete-region (point-min) (point-max))
    (insert-file-contents-literally keats-file)
    (goto-char (point-min))
    (setq query (concat "^\\(.*\\)" keats-delimiter "\\(.*" query ".*\\)$"))
    (while (re-search-forward query nil t)
      (add-to-list 'res (concat (match-string 1) ": " (match-string 2))))
    (cond (res
           (delete-region (point-min) (point-max))
           (dolist (hit res)
             (insert (concat hit "\n"))))
          (t
           (kill-this-buffer)
           (print "No matches")))))

(defun keats-find-key-position (&optional key)
  "Searches `keats-file' for a keyboard sequence. If the
  sequence is found, the beginning line position of that line is
  returned. If there is no match, nil is returned."
  (setq key (keats-key-or-read-key key))
  (switch-to-buffer (get-buffer-create keats-temp-buffer))
  (delete-region (point-min) (point-max))
  (insert-file-contents-literally keats-file)
  (goto-char (point-min))
  (let ((res))
    (if (re-search-forward (concat "^" key keats-delimiter ".*$") nil t)
        (setq res (line-beginning-position)))
    (kill-this-buffer)
    res))

(defun keats-read-key ()
  "Reads a key sequence from the keyboard. To end input, press
RET and key sequence will be returned. And to abort, press C-g
and nil will be returned."
  (let ((cursor-in-echo-area t) (key) (res))
    (setq key (read-key-sequence "Describe key: "))
    (while (not (string-match "RET\\|C-g" (key-description key)))
      (setq res (concat res key))
      (setq key (read-key-sequence (key-description res))))
    (if (string= (key-description key) "RET")
        (unless (string= (key-description res) "")
          (key-description res)))))

(defun keats-file-exists-p ()
  "Returns true if keats file exists. False otherwise."
  (and (file-exists-p keats-file) (not (file-directory-p keats-file))))

(defun keats-file-valid-p ()
  "Returns true if keats file is valid (read and writable). False otherwise."
  (and (file-readable-p keats-file) (file-writable-p keats-file)))

(defun keats-key-or-read-key (key)
  "If KEY is non-nil, KEY is returned. Otherwise a key sequence
is read."
  (or key (setq key (keats-read-key)))
  key)

(define-minor-mode keats-mode
  "Simple interface to Emacs keybinding cheats."
  :init-value nil
  :keymap keats-mode-map
  (cond (keats-mode
         (cond ((not (keats-file-exists-p))
                (switch-to-buffer (get-buffer-create keats-temp-buffer))
                (write-file keats-file nil)
                (kill-this-buffer)))
         (unless (keats-file-valid-p)
           (print "No valid keats file")
           (setq keats-mode nil)))))

(provide 'keats)

;;; keats.el ends here