;;; init.el -- Emacs configuration file -*- lexical-binding: t; -*-

;;; Comment:

;;; Code:
;; + Package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(setq package-native-compile t)
(setq use-package-always-ensure t
      use-package-always-defer t)

;; + Path
(eval-and-compile
  (defconst my-data-directory "~/.emacs.d/data/"
    "Directory to save datas."))
(eval-when-compile
  (unless (file-exists-p my-data-directory)
    (make-directory my-data-directory)))
(defconst my-auto-save-list-directory
  (concat my-data-directory "auto-save-lists/")
  "Directory to save auto-save-lists.")
(defconst my-backup-directory
  (concat my-data-directory "backups/")
  "Directory to save backup files.")

(use-package exec-path-from-shell
  :config
  ((exec-path-from-shell-check-startup-files)
   (exec-path-from-shell-variables . '("PATH")))
  (exec-path-from-shell-initialize))

;; + Editing
(electric-pair-mode +1)

(setq-default indent-tabs-mode nil
	          tab-width 4
	          truncate-lines t
	          line-move-visual t
	          fill-column 80
	          cursor-type 'box
	          require-final-newline t)
(setq global-auto-revert-mode 1)
(delete-selection-mode 1)
(setq show-trailing-whitespace t)

(use-package comment-dwim-2
  :bind ("C-;" . comment-dwim-2))

(use-package mwim
  :bind (("C-a" . mwim-beginning)
         ("C-e" . mwim-end)))

(use-package puni
  :config
  (puni-global-mode))

;; (use-package flyspell
;;   :config
;;   (add-hook 'text-mode-hook 'flyspell-mode)
;;   (add-hook 'prog-mode-hook 'flyspell-prog-mode))

;; (use-package outli)

;; + File
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("w" . wdired-change-to-wdired-mode))
  :config
  (setq dired-dwim-target t
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        delet-by-moving-to-trash t
        dired-isearch-filenames 'dwim
        dired-ls-F-marks-symlinks t))

(global-auto-revert-mode t)

;; + Git
(use-package magit
  :bind ("C-x g" . magit-status))

;; + Completion
(fido-vertical-mode 1)
(ffap-bindings)

(use-package recentf
  :ensure nil
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 30
        recentf-max-menu-items 30))

(use-package hippie-expand
  :ensure nil
  :bind ("M-/" . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

(prefer-coding-system 'utf-8-unix)

;; + Buffer
(use-package bs-show
  :ensure nil
  :bind ("C-x C-b" . bs-show))

;; + Save/Backup/Undo
(save-place-mode t)
(setq make-backup-files t)
(add-to-list 'backup-directory-alist (cons "." my-backup-directory))

(use-package undohist
  :config
  (undohist-initialize))

;; + Shell
(use-package eat
  :custom
  (eat-kill-buffer-on-exit t))

;; + Language
;; |- Julia
(use-package julia-mode)
(use-package julia-repl
  :hook (julia-mode.julia-repl-mode))
(use-package julia-snail)
(set-language-environment "UTF-8")

;; + UI
;; |- mode-line
(column-number-mode t)
(global-display-line-numbers-mode t)
(size-indication-mode t)

(show-paren-mode +1)
(setq show-paren-style 'mixed)

(setq use-short-answer t)
(setq visible-bell t)
(global-font-lock-mode t)

(tab-bar-mode +1)
(tab-bar-history-mode +1)
(setq tab-bar-tab-hints t
      tab-bar-new-button-show nil
      tab-bar-close-button-show nil
      tab-bar-show 1
      tab-bar-tab-name-function 'tab-bar-tab-name-current-with-count)

(use-package display-fill-column-indicator
  :hook (prog-mode.display-fill-column-indicator-mode)
  :config
  (setq-default display-fill-column-indicator 80))

(use-package which-key
  :config
  (which-key-mode))

;; + Keybind
(use-package meow
  :init
  (setq meow-use-cursor-position-hack t)
  :config
  (setq meow-use-clipboard t
        meow-expand-hint-counts t)
  (meow-setup-indicator))
(require 'meow)
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))
(meow-setup)
(meow-global-mode 1)

;; + Theme
(load-theme 'modus-operandi)

;; + Font
(set-face-attribute 'default nil :family "PlemolJP" :height 120)

;; + misc
(setq kill-ring-max 1000)

;;; init.el ends here.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(outli julia-snail julia-repl julia-mode eat magit evil puni meow mwim undohist which-key exec-path-from-shell comment-dwim-2 comment-dwim2 compat)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
