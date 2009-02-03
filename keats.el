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
;; Keat - Is a short for Keyboard Shortcut Cheat.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;; Description ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Keats is a mode that one can say is an interface to a cheats file
;; containing Emacs keyboard shortcuts (shortcut and
;; description). With this mode you can easy add, edit, remove, show
;; and search your keats without having to leave the buffer you are
;; currently working in.
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
;; This mode stores all keats in the file given by the variable
;; `keats-file'. The default value is a hidden file named .keats that
;; will end up in your HOME folder. If you are not happy with that
;; file you can change it yourself:
;; (setq keats-file "~/emacs.d/keats")
;;
;; All keats are by default stored in `keats-file' in the format
;; "Key|Description". The pipe (|) is used as a delimiter. You can
;; change the delimiter by changing the variable `keats-delimiter':
;; (setq keats-delimiter ":")
;;
;; You can not however change this to whatever you want. For example a
;; whitespace will not work since both a key sequence and a
;; description may contain whitespaces. Also be aware of that
;; `keats-delimiter' is used in some regular expressions. This means
;; that if you want to use for example a dot (.) as a delimiter, you
;; must escape it:
;; (setq keats-delimiter "\.")
;;
;; Note that you can not change the value of this variable when you
;; have keats in `keats-file'. You then manually have to update the
;; delimiter.
;;
;; Many of the commands will prompt you for a key sequence. To enter
;; one, start type the sequence and when done press RET (enter). If
;; you want to abort, press C-g.
;;
;; == ADD (C-c k a)
;; Will add a new keat if it does not already
;; exist. If it does exists the edit action will be called with the
;; same key. Read below under edit.
;;
;; == EDIT (C-c k e)
;; Edits an already existing keat.
;;
;; == DESCRIPTION (C-c k d)
;; Print the description for a keat.
;;
;; == SEARCH
;; Searches regularly, without respect to case, in description for a
;; keyword. A new buffer containing all hits is created. If none is
;; found, a message is printed.
;;
;; == REMOVE (C-c k r)
;; Removes a keat.
;;
;; Note that even thought this might not be a common usage, all of the
;; above action can be called from a lisp program:
;; (keats-add "C-x C-f" "Opens file")
;; (keats-edit "C-x C-f" "Opens file is a new buffer")
;; (keats-print-description "C-x C-f")
;; (keats-search "buffer")
;; (keats-remove "C-x C-f")
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defvar keats-temp-buffer "*keats*"
  "Temp buffer.")

(defvar keats-list '()
  "List of plists where each plist is a keat.")

(defcustom keats-file "~/.keats"
  "Path to file where keats are stored."
  :group 'keats)

(defcustom keats-delimiter "|"
  "The delimiter used in `keats-file' to separate key sequence
  from description. Don't change this when you already have
  contents in you `keats-file'. If you do this, old keats will
  not be recognized."
  :group 'keats)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun keats-add (&optional key description)
  "Adds a keat if it does not already exist. If it exists,
`keats-edit' is called for key if user confirms."
  (interactive)
  (setq key (or key (keats-read-key)))
  (if key
      (cond ((keats-key-exists key)
             (if (yes-or-no-p "Already exists. Do you want to edit it? ")
                 (keats-edit key)))
            (t
             (setq description (or description (read-string "Description: ")))
             (add-to-list 'keats-list `(:key ,key :description ,description))
             (print (concat key " added"))))))

(defun keats-edit (&optional key description)
  "Edits a keat in `keats-file' if it exists."
  (interactive)
  (setq key (or key (keats-read-key)))
  (let ((line)
        (pos (keats-find-key-position key))
        (old-description (keats-get-description key)))
    (cond ((and key pos)
           (or description (setq description (read-string "Description: " old-description)))
           (find-file keats-file)
           (goto-char pos)
           (replace-string old-description description nil (line-beginning-position) (line-end-position))
           (save-buffer)
           (kill-this-buffer)
           (print (concat "Updated " key)))
          (t
           (print (concat key " not found"))))))

(defun keats-remove (&optional key)
  "Removes the given key sequence from `keats-file'."
  (interactive)
  (setq key (or key (keats-read-key)))
  (let ((pos (keats-find-key-position key)))
    (cond ((and key pos (yes-or-no-p (concat "Are you sure you want to remove " key " ? ")))
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
  (setq key (or key (keats-read-key)))
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
  (setq key (or key (keats-read-key)))
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

(defun keats-find-key-position (key)
  "Searches `keats-file' for a keyboard sequence. If the
  sequence is found, the beginning line position of that line is
  returned. If there is no match, nil is returned."
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

(defun keats-key-exists (key)
  "Returns t if key exists. False otherwise."
  (let ((list keats-list))
    (while (and (not (string= (plist-get (car list) :key) key)) list)
      (setq list (cdr list)))
    (not (not list))))

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