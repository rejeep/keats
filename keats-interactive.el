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
;; This mode is an extension to `keats-mode'. This mode adds extends
;; Keats mode with an interactive part.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Commentary ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; You are most likely to use this mode if you want to add, edit or
;; remove many keats at the same time. In this mode these keybindings
;; are set:
;;  * a - Adds a new keat
;;  * e - Edits keat at point
;;  * r - Removes keat at point
;;  * n - Move one step down
;;  * p - Move one step up
;;  * q - Quit the buffer and mode
;;  * w - Writes keats to file
;;  * RET - Runs command for which key at point is connected to
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(require 'keats)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst keats-interactive-temp-buffer "*Keats Interactive*"
  "Temp buffer.")

(defvar keats-interactive-title-height nil
  "We must know how high the title is so that we don't enter it's
  area.")

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
  "Adds a new keat if it does not already exists."
  (interactive)
  (let* ((key (keats-read-key))
         (keat (keats-key-exists key)))
    (if key
        (if keat
            (message "%s already exist. Edit instead." key)
          (let ((keat (keats-add key)))
            (if keat
                (keats-interactive-insert-keat keat)))))))

(defun keats-interactive-edit ()
  "Edits keat at point."
  (interactive)
  (let* ((key (keats-interactive-key-at-point))
         (keat (keats-key-exists key)))
    (cond ((and keat (keats-edit key))
           (delete-region (line-beginning-position) (line-end-position))
           (keats-interactive-insert-keat keat (line-beginning-position))))))

(defun keats-interactive-remove ()
  "Removes keat at point."
  (interactive)
  (cond ((keats-remove (keats-interactive-key-at-point))
         (delete-region (line-beginning-position) (line-end-position))
         (delete-char 1)
         (keats-interactive-previous))))

(defun keats-interactive-next ()
  "Moves one step down in the list of keats."
  (interactive)
  (keats-interactive-move
   (lambda ()
     (if (< (line-number-at-pos nil) (count-lines (point-min) (point-max)))
         (next-line)))))

(defun keats-interactive-previous ()
  "Moves one step down in the list of keats."
  (interactive)
  (keats-interactive-move
   (lambda ()
     (if (> (line-number-at-pos nil) (1+ keats-interactive-title-height))
         (previous-line)))))

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

(defun keats-interactive-move (function)
  "Helper for moving up and down in list. Makes sure that correct
lines are highlighted."
  (keats-interactive-put-line-property 'face nil)
  (funcall function)
  (keats-interactive-put-line-property 'face 'keats-highlight))

(defun keats-interactive-insert-keat (keat &optional pos)
  "Inserts a keat at POS or if POS is nil last in the list."
  (save-excursion
    (goto-char (or pos (point-max)))
    (unless (= (current-column) 0)
      (insert "\n"))
    (insert (keats-to-string keat)))
  (keats-interactive-put-line-property 'face 'keats-highlight))

(defun keats-interactive-insert-keats (keats &optional pos)
  "Inserts a list of keats. See `keats-interactive-insert-keat'."
  (dolist (keat keats)
    (keats-interactive-insert-keat keat pos)))

(defun keats-interactive-key-at-point ()
  "Returns key at point."
  (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
    (string-match "^\\(.*\\):" line)
    (match-string-no-properties 1 line)))

(defun keats-interactive-put-line-property (prop val &optional beg end)
  "Changes the face of the current line."
  (put-text-property (or beg (line-beginning-position)) (or end (line-end-position)) prop val))

(defun keats-interactive-set-title (title)
  "Sets the title."
  (goto-char (point-min))
  (delete-region (line-beginning-position) (line-end-position))
  (insert title)
  (let ((min (point-min)) (max (point-max)))
    (setq keats-interactive-title-height (count-lines min max))
    (keats-interactive-put-line-property 'face 'keats-title min max))
  (newline))

(defun keats-interactive-mode (title)
  "Major mode to interactively manage Keats."
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