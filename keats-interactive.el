;;; keats-interactive.el --- Interactive mode for Keats mode.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;; Description ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
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
;; (require 'keats-interactive)
;;
;; Then start it:
;; (keats-interactive-mode t) or M-x keats-interactive-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Commentary ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(require 'keats)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst keats-interactive-temp-buffer "*Keats Interactive*"
  "Temp buffer.")

(defvar keats-interactive-mode-hook '()
  "Hook for this mode. Is evaluated last in mode startup.")

(defvar keats-interactive-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'keats-interactive-add)
    (define-key map (kbd "e") 'keats-interactive-edit)
    (define-key map (kbd "r") 'keats-interactive-remove)
    (define-key map (kbd "n") 'keats-interactive-next)
    (define-key map (kbd "p") 'keats-interactive-previous)
    (define-key map (kbd "q") 'keats-interactive-quit)
    (define-key map (kbd "w") 'keats-interactive-write)
    (define-key map (kbd "RET") 'keats-interactive-run)
    map)
  "Keymap for `keats-interactive-mode'.")

(defface keats-title
  '((((class color) (background dark))
     :foreground "red"
     :bold t))
  "Face for title."
  :group 'keats)

(defface keats-highlight
  '((((class color) (background light))
     :background "gray95")
    (((class color) (background dark))
     :background "dim gray"))
  "Face for active line."
  :group 'keats)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun keats-interactive-add ()
  ""
  (interactive)
  )

(defun keats-interactive-edit ()
  ""
  (interactive)
  )

(defun keats-interactive-remove ()
  ""
  (interactive)
  )

(defun keats-interactive-next ()
  ""
  (interactive)
  )

(defun keats-interactive-previous ()
  ""
  (interactive)
  )

(defun keats-interactive-quit ()
  "Exits mode by closing buffer."
  (interactive)
  (kill-this-buffer))

(defun keats-interactive-write ()
  "Writes keats to file."
  (interactive)
  (keats-write))

(defun keats-interactive-run ()
  "Runs command for with key at point is connected to."
  (interactive)
  (let* ((key (keats-interactive-key-at-point))
         (function (key-binding (read-kbd-macro key))))
    (cond (function
           (kill-this-buffer)
           (call-interactively function))
          (t
           (message "%s runs no command" key)))))

(defun keats-interactive-key-at-point ()
  "Returns key at point."
  (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
    (string-match "^\\(.*\\):" line)
    (match-string-no-properties 1 line)))

(defun keats-interactive-put-line-property (prop val)
  "Changes the face of the current line."
  (put-text-property (line-beginning-position) (line-end-position) prop val))

(defun keats-interactive-set-title (title)
  "Sets the title."
  (goto-char (point-min))
  (delete-region (line-beginning-position) (line-end-position))
  (insert title)
  (keats-interactive-put-line-property 'face 'keats-title)
  (newline))

(defun keats-interactive-mode (title)
  "Major mode to interactively manage Keats."
  (interactive "*sTitle: ")
  (switch-to-buffer (get-buffer-create keats-interactive-temp-buffer))
  (delete-region (point-min) (point-max))
  (kill-all-local-variables)
  (use-local-map keats-interactive-mode-map)
  (setq mode-name "Keats Interactive")
  (setq major-mode 'keats-interactive-mode)
  (keats-interactive-set-title title)
  (run-mode-hooks 'keats-interactive-mode-hook))

(provide 'keats-interactive)

;;; keats.el ends here