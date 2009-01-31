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

(defvar keats-file "~/.keats"
  "Path to file where keats are stored.")

(defvar keats-temp-buffer "*keats*"
  "Temp buffer.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun keats-add ()
  (interactive)
  ""
  )

(defun keats-edit ()
  (interactive)
  ""
  )

(defun keats-delete ()
  (interactive)
  ""
  )

(defun keats-get-description ()
  (interactive)
  ""
  )

(defun keats-search ()
  (interactive)
  ""
  )

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

(defun keats-find-key-position (&optional key)
  "Searches `keats-file' for a keyboard sequence. If the
  sequence is found, the beginning line position of that line is
  returned. If there is no match, nil is returned."
  (or key (setq key (keats-read-key)))
  (switch-to-buffer (get-buffer-create keats-temp-buffer))
  (delete-region (point-min) (point-max))
  (insert-file-contents-literally keats-file)
  (beginning-of-buffer)
  (let ((res))
    (if (re-search-forward (concat "^" key "|.*$") nil t)
        (setq res (line-beginning-position)))
    (kill-this-buffer)
    res))

(defun keats-file-exists-p ()
  "Returns true if keats file exists. False otherwise."
  (and (file-exists-p keats-file) (not (file-directory-p keats-file))))

(defun keats-file-valid-p ()
  "Returns true if keats file is valid (read and writable). False otherwise."
  (and (file-readable-p keats-file) (file-writable-p keats-file)))

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