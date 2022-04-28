;;; early-init.el -- early configuration

;; -*- lexical-binding: t -*-
;; -*- no-byte-compile: t -*-

;;; Code:


(setq load-prefer-newer t)

;; (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
;;   (add-to-list 'load-path (expand-file-name "lib/packed" dir))
;;   (add-to-list 'load-path (expand-file-name "lib/auto-compile" dir))
;;   (add-to-list 'load-path (expand-file-name "lib/https:/github.com/zk-phi/setup" dir)))
;; (require 'auto-compile)
;; (auto-compile-on-load-mode)
;; (auto-compile-on-save-mode)

(setq package-enable-at-startup nil)

;; (with-eval-after-load 'package
;;   (add-to-list 'package-archives
;;                (cons "melpa" "https://melpa.org/packages/")
;;                t)
;;   (add-to-list 'package-archives
;;                (cons "org" "https://orgmode.org/elpa/")
;;                t))
(setq initial-frame-alist '((fullscreen . maximized)))
;; (custom-set-variables '(initial-frame-alist '((fullscreen . maximized))))
;; (setq default-frame-alist
;;       '((width . 90)
;;         (height . 50)))
(setq default-frame-alist　'((fullscreen . fullheight)))

;; End:
;;; early-init.el ends here
