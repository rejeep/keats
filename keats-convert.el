;;; keats-convert.el --- Converts the keats file from the old format to the new.

;; Keats has switched to a new format when storing the keats. The old
;; format is stupid and inefficient.
;;
;; The old format was, as standard, on this form:
;; key|description
;;
;; The new format looks like this:
;; (
;;   ...
;;   (:key "key" :description "description")
;;   ...
;; )
;;
;; Note: When you run `keats-convert' the old file will not be
;; replaced. So you don't have to worry about lost keats.
;;
;; To convert from the old format to the new. Enter this file and eval
;; the function `keats-convert'. Easiest way to do this is to eval the
;; whole buffer:
;; M-x eval-buffer
;; The function `keats-convert' should now be visible.
;;
;; Now run the function `keats-convert' to get a new buffer with all
;; keats in the new format:
;; M-x keats-convert
;;
;; Enter the delimiter you use in the old keats file and then press
;; enter. A new buffer will appear with all keats in the new
;; format. Now save this file as your new keats file.

(defun keats-convert (delimiter)
  "Converts from the old format to the new."
  (interactive "sDelimiter: ")
  
  (switch-to-buffer (get-buffer-create "keats-convert"))
  (delete-region (point-min) (point-max))
  (insert-file keats-file)
  (goto-char (point-min))
  
  (let ((keat) (key) (description) (temp-list))
    
    (while (re-search-forward (concat "^\\(.*\\)" delimiter "\\(.*\\)$") nil t)
      (setq key (match-string-no-properties 1))
      (setq description (match-string-no-properties 2))
      
      (setq keat `(:key ,key :description ,description))
      (add-to-list 'temp-list keat t))
    
    (delete-region (point-min) (point-max))
    (insert (pp-to-string temp-list))))