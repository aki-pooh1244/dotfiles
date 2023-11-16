;;; early-init.el -- early configuration

;; -*- lexical-binding: t -*-
;; -*- no-byte-compile: t -*-

;;; Code:


(setq load-prefer-newer t)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "site-lisp" dir))
  (add-to-list 'load-path (expand-file-name "el-get" dir)))

(setq package-enable-at-startup nil)

;; (with-eval-after-load 'package
;;   (add-to-list 'package-archives
;;                (cons "melpa" "https://melpa.org/packages/")
;;                t)
;;   (add-to-list 'package-archives
;;                (cons "org" "https://orgmode.org/elpa/")
;;                t))
;; (setq initial-frame-alist '((fullscreen . maximized)))
(setq initial-frame-alist '((width . 100)
                            (height . 60)))
;; (custom-set-variables '(initial-frame-alist '((fullscreen . maximized))))
;; (setq default-frame-alist
;;       '((width . 90)
;;         (height . 50)))
(setq default-frame-alist '((width . 100)
                            (height . 60)))

;; End:
;;; early-init.el ends here
