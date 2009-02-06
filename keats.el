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
;; Keat - Is a short for Keybinding Cheat.
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
;; description may contain white spaces. Also be aware of that
;; `keats-delimiter' is used in some regular expressions. This means
;; that if you want to use for example a dot (.) as a delimiter, you
;; must escape it:
;; (setq keats-delimiter "\.")
;;
;; Note that you can not change the value of this variable when you
;; have keats in `keats-file'. You then manually have to update the
;; delimiter in that file.
;;
;; For each add, edit and delete `keats-save-count' is increased by
;; one if `keats-save-at' is non nil. When `keats-save-count' is
;; (larger or) equal to `keats-save-at', `keats-list' is written to
;; `keats-file'. You can change the value of `keats-save-at' if you
;; want to write to file more less or often, or not at all. nil value
;; means to not auto save at all.
;;
;; Many of the commands will prompt you for a key sequence. To enter
;; one, start type the sequence and when done press RET (enter). If
;; you want to abort, press C-g.
;;
;; == ADD (C-c k a)
;; Will add a new keat if it does not already exist. If it does exists
;; the edit action will be called with the same key. Read below under
;; edit.
;;
;; == EDIT (C-c k e)
;; Edits an already existing keat.
;;
;; == DESCRIPTION (C-c k d)
;; Prints the description for a key sequence.
;;
;; == SEARCH
;; Searches regularly, without respect to case, in description for a
;; given regexp. If none is found, a message is printed. If there's
;; any hit, a new buffer containing all hits is created. In this
;; buffer you can use n and p to move up and down between the
;; different keats. q kills the buffer. And RET calls the function
;; that the key on the current line responds to interactively.
;;
;; == REMOVE (C-c k r)
;; Removes a keat.
;;
;; == WRITE (C-c k w)
;; Writes `keats-list' to file. This is done every time Emacs is
;; killed.
;;
;; Note that even though this might not be a common usage, all of the
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
  (define-key map (kbd "s") 'keats-search)
  (define-key map (kbd "w") 'keats-write))

(defvar keats-temp-buffer "*keats*"
  "Temp buffer.")

(defvar keats-list '()
  "List of plists where each plist is a keat on the form (:key
\"key\" :description \"description\")")

(defvar keats-save-count 0
  "Holds the value for how many changes there has been since last
  save.")

(defcustom keats-file "~/.keats"
  "Path to file where keats are stored."
  :group 'keats)

(defcustom keats-delimiter "|"
  "The delimiter used in `keats-file' to separate key sequence
  from description. Don't change this when you already have
  contents in you `keats-file'. If you do this, old keats will
  not be recognized."
  :group 'keats)

(defcustom keats-save-at 5
  "Tells how many changes (add, edit and remove) there can be
without auto saving. nil value means no auto saving."
  :group 'keats)

(defface keats-highlight
  '((((class color) (background light))
     :background "gray95")
    (((class color) (background dark))
     :background "dim gray"))
  "Face for active line in search."
  :group 'keats)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun keats-add (&optional key description)
  "Adds a keat if it does not already exist. If it exists,
`keats-edit' is called for key if user confirms."
  (interactive)
  (setq key (or key (keats-read-key)))
  (if key
      (cond ((keats-key-exists key)
             (if (yes-or-no-p (concat key " already exists. Do you want to edit it? "))
                 (keats-edit key)))
            (t
             (setq description (or description (read-string "Description: ")))
             (cond (description
                    (add-to-list 'keats-list `(:key ,key :description ,description) t)
                    (keats-update-save)
                    (message "%s added" key)))))))

(defun keats-edit (&optional key description)
  "Edit the description of an already existing keat."
  (interactive)
  (setq key (or key (keats-read-key)))
  (if key
      (let ((keat (keats-key-exists key)))
        (cond (keat
               (setq description (or description (read-string "Description: " (plist-get keat :description))))
               (cond (description
                      (plist-put keat :description description)
                      (keats-update-save)
                      (message "%s updated" key))))
              (t
               (message "%s not found" key))))))

(defun keats-remove (&optional key)
  "Removes a keat from the list."
  (interactive)
  (setq key (or key (keats-read-key)))
  (if key
      (let ((keat (keats-key-exists key)))
        (cond ((and keat (yes-or-no-p (concat "Are you sure you want to remove " key "?")))
               (setq keats-list (remove keat keats-list))
               (keats-update-save)
               (message "%s removed" key))
              (t
               (message "%s not found" key))))))

(defun keats-print-description (&optional key)
  "Prints the description of the given key sequence."
  (interactive)
  (setq key (or key (keats-read-key)))
  (if key
      (let ((keat (keats-key-exists key)))
        (if keat
            (message (plist-get keat :description))
          (message "%s not found" key)))))

(defun keats-search (query)
  "Searches for keats that matches QUERY as description."
  (interactive "*sQuery: ")
  (let ((matches '()))
    (dolist (keat keats-list)
      (if (string-match query (plist-get keat :description))
          (add-to-list 'matches (keats-to-string keat))))
    (cond (matches
           (switch-to-buffer (get-buffer-create keats-temp-buffer))
           (delete-region (point-min) (point-max))
           (insert (concat "Matches for: '" query "'\n"))
           (dolist (match matches)
             (insert (concat match "\n")))
           (backward-delete-char 1)
           (goto-line 2)
           (goto-char (line-beginning-position))
           (keats-put-line-property 'face 'keats-highlight)
           (local-set-key (kbd "n") (lambda ()
                                      (interactive)
                                      (keats-put-line-property 'face nil)
                                      (if (< (line-number-at-pos nil) (count-lines (point-min) (point-max))) (next-line))
                                      (keats-put-line-property 'face 'keats-highlight)))
           (local-set-key (kbd "p") (lambda ()
                                      (interactive)
                                      (keats-put-line-property 'face nil)
                                      (if (> (line-number-at-pos nil) 2) (previous-line))
                                      (keats-put-line-property 'face 'keats-highlight)))
           (local-set-key (kbd "q") 'kill-this-buffer)
           (local-set-key (kbd "RET") (lambda ()
                                        (interactive)
                                        (let ((line (buffer-substring (line-beginning-position) (line-end-position))) (function))
                                          (string-match "^\\(.*\\):" line)
                                          (setq function (key-binding (read-kbd-macro (match-string 1 line))))
                                          (cond (function
                                                 (kill-this-buffer)
                                                 (call-interactively function))
                                                (t
                                                 (print "Key runs no command"))))))))))

(defun keats-write ()
  "Writes `keats-list' to `keats-file'."
  (interactive)
  (find-file-literally keats-file)
  (delete-region (point-min) (point-max))
  (dolist (plist keats-list)
    (insert (concat (plist-get plist :key) keats-delimiter (plist-get plist :description) "\n")))
  (save-buffer)
  (kill-this-buffer)
  (setq keats-save-count 0))

(defun keats-read-key ()
  "Reads a key sequence from the keyboard. To end input, press
RET and key sequence will be returned. And to abort, press C-g
and nil will be returned."
  (let ((cursor-in-echo-area t) (key) (res))
    (setq key (read-key-sequence-vector "Describe key: "))
    (while (not (string-match "RET\\|C-g" (key-description key)))
      (setq res (vconcat res key))
      (setq key (read-key-sequence-vector (key-description res))))
    (if (string= (key-description key) "RET")
        (unless (string= (key-description res) "")
          (key-description res)))))

(defun keats-to-string (keat)
  "Returns a string representation of a keat."
  (concat (plist-get keat :key) ": " (plist-get keat :description)))

(defun keats-file-exists-p ()
  "Returns true if keats file exists. False otherwise."
  (and (file-exists-p keats-file) (not (file-directory-p keats-file))))

(defun keats-file-valid-p ()
  "Returns true if keats file is valid (read and writable). False otherwise."
  (and (file-readable-p keats-file) (file-writable-p keats-file)))

(defun keats-key-exists (key)
  "Returns t if key exists. False otherwise."
  (if key
      (let ((list keats-list))
        (while (and (not (string= (plist-get (car list) :key) key)) list)
          (setq list (cdr list)))
        (car list))))

(defun keats-update-save ()
  "First increases the number of updates. Then writes to file if
there has been enough changes. But only if `keats-save-at' is non nil."
  (cond (keats-save-at
         (setq keats-save-count (1+ keats-save-count))
         (if (>= keats-save-count keats-save-at)
             (keats-write)))))

(defun keats-put-line-property (prop val)
  "Changes the face of the current line."
  (put-text-property (line-beginning-position) (line-end-position) prop val))

(define-minor-mode keats-mode
  "Simple interface to Emacs keybinding cheats."
  :init-value nil
  :keymap keats-mode-map
  (cond (keats-mode
         (cond ((not (keats-file-exists-p))
                (switch-to-buffer (get-buffer-create keats-temp-buffer))
                (delete-region (point-min) (point-max))
                (write-file keats-file nil)
                (kill-this-buffer)))
         (cond ((keats-file-valid-p)
                (switch-to-buffer (get-buffer-create keats-temp-buffer))
                (delete-region (point-min) (point-max))
                (insert-file keats-file)
                (goto-char (point-min))
                (while (re-search-forward (concat "^\\(.*\\)" keats-delimiter "\\(.*\\)$") nil t)
                  (add-to-list 'keats-list `(:key ,(match-string-no-properties 1) :description ,(match-string-no-properties 2))))
                (kill-this-buffer)
                (add-hook 'kill-emacs-hook
                          '(lambda()
                             (keats-write)))))
         (unless (keats-file-valid-p)
           (error "No valid keats file")))))

(provide 'keats)

;;; keats.el ends here