;;; init.el -- init config

;;; Code:

;; borg
(add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
(require 'borg)
(borg-initialize)
