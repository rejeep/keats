(let ((current-directory (file-name-directory load-file-name)))
  (setq keats-root-path (expand-file-name ".." current-directory))
  (setq keats-util-path (expand-file-name "util" keats-root-path)))

(add-to-list 'load-path keats-root-path)
(add-to-list 'load-path (expand-file-name "espuds" keats-util-path))

(require 'keats)
(require 'espuds)
