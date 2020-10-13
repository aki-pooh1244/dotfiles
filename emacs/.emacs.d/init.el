;;; init.el  -*- lexical-binding: t; -*-

;; Reference doom-emacs
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly

;;; Code:

;; early init load optimizations
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Load readme.org
(require 'org)
(org-babel-load-file
    (expand-file-name
       (concat user-emacs-directory "readme.org")))

;; reset gc
(add-hook 'emacs-startup-hook
    (lambda ()
      (setq gc-cons-threshold 16777216 ; 16mb
            gc-cons-percentage 0.1)))

(add-hook 'emacs-startup-hook
    (lambda ()
      (setq file-name-handler-alist startup/file-name-handler-alist)))    
      
;;; End here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-mode-lighter "")
 '(anzu-search-threshold 1000)
 '(company-echo-delay 0 t)
 '(company-idle-delay 0 t)
 '(company-minimum-prefix-length 1 t)
 '(company-quickhelp-delay 0.8 t)
 '(company-show-numbers t t)
 '(custom-safe-themes
   '("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default))
 '(inferior-lisp-program "ros -Q run" t)
 '(package-selected-packages
   '(ido-complete-space-or-hyphen crm-custom ido-sort-mtime skk-study helm-swoop company-tabnine company-posframe company-box visual-regexp-steroids zoom-window elscreen-separate-buffer-list projectile company-quickhelp company-math popwin rainbow-delimiters rainbow-mode which-key-posframe use-package quelpa pretty-mode mwim diminish all-the-icons))
 '(time-stamp-active t t)
 '(time-stamp-end ">" t)
 '(time-stamp-format "%:y-%03:b-%02d %02H:%02M:%02S from %s by %u" t)
 '(time-stamp-line-limit 20 t)
 '(time-stamp-start "[lL]ast[ -][uU]pdated[ 	]*:[ 	]*<" t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
