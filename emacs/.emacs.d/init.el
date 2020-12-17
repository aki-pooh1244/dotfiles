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
 '(beacon-mode t)
 '(company-echo-delay 0 t)
 '(company-idle-delay 0 t)
 '(company-minimum-prefix-length 1 t)
 '(company-quickhelp-delay 0.8 t)
 '(company-show-numbers t t)
 '(custom-safe-themes
   '("2229862b727e0caa3b0a53152ef82d37819324bf5d2c657f859b2b0a475b34f7" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "7d23729342514203c5a1dc4c8f1f67d96ac65f660dd73cb8c6bfffc8eba79804" "f490984d405f1a97418a92f478218b8e4bcc188cf353e5dd5d5acd2f8efd0790" "28a104f642d09d3e5c62ce3464ea2c143b9130167282ea97ddcc3607b381823f" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default))
 '(inferior-lisp-program "ros -Q run" t)
 '(mini-frame-show-parameters '((top . 10) (width . 0.7) (left . 0.5)))
 '(package-selected-packages
   '(ido-complete-space-or-hyphen crm-custom ido-sort-mtime skk-study helm-swoop company-tabnine company-posframe company-box visual-regexp-steroids zoom-window elscreen-separate-buffer-list projectile company-quickhelp company-math popwin rainbow-delimiters rainbow-mode which-key-posframe use-package quelpa pretty-mode mwim diminish all-the-icons))
 '(time-stamp-active t t)
 '(time-stamp-end ">" t)
 '(time-stamp-format "%:y-%03:b-%02d %02H:%02M:%02S from %s by %u" t)
 '(time-stamp-line-limit 20 t)
 '(time-stamp-start "[lL]ast[ -][uU]pdated[ 	]*:[ 	]*<" t)
 '(warning-suppress-types '((use-package) (use-package)))
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
