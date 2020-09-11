;; -*- lexical-binding: t -*-

;; (require 'package)
;; (setq package-enable-at-startup nil)
;; (unless (assoc-default "melpa" package-archives)
;;    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
;; (unless (assoc-default "org" package-archives)
;;    (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))
;; (unless package--initialized (package-initialize))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; Install use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; (unless (package-installed-p 'use-package)
;;         (package-refresh-contents)
;; 	    (package-install 'use-package)
;;         (package-install 'diminish)
;;         (package-install 'bind-key))
;; (eval-when-compile
;;     (require 'use-package)
;;     (require 'diminish)
;;     (require 'bind-key))
;; (setq use-package-always-ensure t)
;; (setq use-package-expand-minimally t)

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(defun display-startup-echo-area-message ()
  (message " "))
(setq initial-scratch-message nil)

(use-package server
  :straight nil
  :config
  (unless (server-running-p)
    (server-start)))

(defun ai/reload-emacs-configulation ()
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
    "Update `load-path'."
    (push (expand-file-name "elisp" user-emacs-directory) load-path)
    (push (expand-file-name "straight" user-emacs-directory) load-path))

(defun add-subdirs-to-load-path (&rest _)
    "Add subdirectories to `load-path'."
    (let ((default-directory
       (expand-file-name "elisp" user-emacs-directory)))
        (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package f)
(use-package s)

(setq-default cursor-type 'bar)
(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode +1)
(delete-selection-mode t)
(size-indication-mode t)
(display-line-numbers-mode -1)
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)

(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; (setq frame-title-format
;;     '((:eval (if (buffer-file-name)
;;                  (abbreviation-file-name (buffer-file-name))
;;                "%b"))))

(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; (setq show-paren-style 'parethesis)
;; (show-paren-mode +1)

(electric-pair-mode +1)

(fset 'yes-or-no-p 'y-or-n-p)

(setq ring-bell-function 'ignore)
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)
;; (global-whitespace-mode +1)
(setq-default tab-width 4 indent-tabs-mode nil)
(setq vc-follow-symlinks t)
(setq tab-always-indent 'complete)
(setq blink-matching-paren nil)

(use-package crux
  :defer t
  :diminish
  :bind
  ("C-c o" . crux-open-with)
  ("C-k" . crux-kill-whole-line)
  ("s-j" . crux-top-join-line)
  ("C-<backspace>" . crux-kill-line-backwards)
  ("s-r" . crux-recentf-ido-find-file)
  ("C-c ," . crux-find-user-custom-file)
  ("C-c e" . crux-eval-and-replace)
  :config
  (crux-with-region-or-buffer indent-region))

(use-package comment-dwim-2
  :defer t
  :diminish
  :bind
  ("M-;" . comment-dwim-2))

(use-package smartparens
  :defer t
  :diminish
  ;; :hook
  ;; (after-init . show-smartparens-global-mode)
  :config
  (require 'smartparens-config)
  ;; (sp-pair "=" "=" :actions '(wrap))
  ;; (sp-pair "+" "+" :actions '(wrap))
  ;; (sp-pair "<" ">" :actions '(wrap))
  ;; (sp-pair "$" "$" :actions '(wrap))
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1))

(use-package paren-completer
  :diminish
  :bind
  ("M-)" . paren-completer-add-single-delimiter))

(use-package smart-newline
  :defer t
  :diminish
  :hook
  (after-init . smart-newline-mode)
  :bind
  ("C-m" . smart-newline)
  :config
  (smart-newline-mode 1)
  (defadvice smart-newline (around C-u activate)
    "C-uを押したら元のC-mの挙動をするようにした.org-modeなどで活用."
    (if (not current-prefix-arg)
        ad-do-it
      (let (current-prefix-arg)
        (let (smart-newline-mode)
          (call-interactively (key-binding (kbd "C-m"))))))))

(use-package bs-show
  :straight nil
  :bind
  ("C-x C-b" . bs-show)
  ;; ("C-x b" , bs-show)
  ("M-]" . bs-cycle-next)
  ("M-[" . bs-cycle-previous))
(use-package bs-ext
  :defer t
  :diminish)

(use-package persistent-scratch
  :defer t
  :diminish
  :config
  (persistent-scratch-setup-default))

(use-package electric-operator
  :diminish
  :hook
  (c-mode-hook . electric-operator-mode)
  (c++-mode-hook . electric-operator-mode)
  (python-mode-hook . electric-operator-mode)
  (perl-mode-hook . electric-operator-mode))

(use-package aggressive-indent
  :defer t
  :diminish
  :config
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode))

(define-key global-map [?¥] [?\\])
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(define-key global-map (kbd "C-x ?") 'help-command)
(define-key global-map (kbd "C-m") 'newline-and-indent)
(define-key global-map (kbd "C-M-m") 'electric-newline-and-maybe-indent)
(define-key global-map (kbd "C-t") 'other-window)
(define-key global-map (kbd "M-r") 'rename-file)
(define-key global-map (kbd "M-:") 'comment-dwim)
(define-key global-map (kbd "C-x |") 'split-window-horizontally)
(define-key global-map (kbd "C-x -") 'split-window-vertically)
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta))
  (setq ns-option-modifier (quote super)))

(use-package which-key
  :diminish
  :hook (after-init . which-key-mode)
  :config
  (use-package which-key-posframe
    :hook (which-key-mode . which-key-posframe-mode)))

(use-package key-chord
  :diminish
  :hook
  (after-init-hook . key-chord-mode)
  :config
  (setq key-chord-two-keys-delay 0.15
        key-chord-one-key-delay  0.15
        key-chord-safety-interval-backward 0.1
        key-chord-safety-interval-forward 0.25)
  (key-chord-define-global "zz" 'undo-fu-only-undo)
  (key-chord-define-global "rr" 'undo-fu-only-redo)
  (key-chord-define-global "ee" 'hippie-expand)
  (key-chord-define-global ",," 'indent-for-comment)
  (key-chord-define-global "jj" 'avy-goto-word-1)
  (key-chord-define-global "jl" 'avy-goto-line)
  (key-chord-define-global "jk" 'avy-goto-char)
  (key-chord-define-global "xx" 'execute-extended-command)
  (key-chord-define-global "yy" 'browse-kill-ring)
  (key-chord-define-global "mc" 'mc/mark-all-dwim)
  (key-chord-mode +1))

;; (use-package hydra)
;; (use-package hydra-posframe
;;   :straight (hydra-posframe :host github
;;                           :repo "Ladicle/hydra-posframe"
;;                           :branch "master")
;;   :hook
;;   (after-init . hydra-posframe-enable))

;; (use-package major-mode-hydra
;;   :bind
;;   ("M-SPC" . major-mode-hydra))

(use-package prescient
  :diminish
  :config
  (prescient-persist-mode +1)
  (setq prescient-history-length 1000))

;; (use-package dash)

;; (use-package s)

(use-package ddskk
  :defer t
  :bind
  ("C-x j" . skk-mode)
  :init
  (setq skk-init-file "~/.skk")
  (setq default-input-method "japanese-skk")
  :config
  (setq skk-byte-complile-init-file t))

;; 余分なメッセージを削除しておきましょう
(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))
(use-package recentf
  :defer t
  :config
  (setq recentf-save-file "~/.emacs.d/.recentf")
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '(".recentf"))
  (setq recentf-auto-cleanup 'never)
  (run-with-idle-timer 30 t
                       '(lambda ()
                          (with-suppressed-message
                           (recentf-save-list)))))
(use-package recentf-ext
  :defer t
  :diminish
  :bind
  ("C-c c o" . recentf-open-files))

(use-package super-save
  :defer t
  :diminish
  :hook
  (after-init-hook . super-save-mode)
  :config
  (setq super-save-auto-save-when-idle t)
  (super-save-mode +1))
(use-package save-place
  :straight nil
  :defer t
  :diminish
  :config
  (save-place-mode 1))
(use-package savehist-mode
  :straight nil
  :defer t
  :diminish
  :config
  (savehist-mode 1)
  (push 'kill-ring savehist-additional-variables)
  (push 'command-history savehist-ignored-variables))

(use-package undo-fu
  :defer t
  :diminish
  :bind
  ("C-/" . undo-fu-only-undo)
  ("M-/" . undo-fu-only-redo))
(use-package undo-fu-session
  :defer t
  :diminish
  :hook
  (after-init-hook . undo-fu-session-mode))

(use-package lusty-explorer
  :defer t
  :diminish
  :config
  (lusty-explorer-mode 1))
(use-package direx
  :diminish
  :bind
  ("C-x C-j" . direx:jump-to-directory))
(setq dired-listing-switches "-alh")

(use-package peep-dired
  :defer t
  :diminish
  :bind
  (:map dired-mode-map
        ("P" . peep-dired)))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)
(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package time-stamp
  :straight nil
  :hook
  (before-save-hook . time-stamp)
  :config
  (setq time-stamp-active t)
  (setq time-stamp-start "[lL]ast[ -][uU]pdated[ \t]*:[ \t]*<")
  (setq time-stamp-format "%:y-%:b-%02d %02H:%02M:%02S")
  (setq time-stamp-end ">")
  (setq time-stamp-line-limit 20))

(add-hook 'after-save-hook
          (lambda ()
            (let ((orig-fg (face-background 'mode-line)))
              (set-face-background 'mode-line "dark green")
              (run-with-idle-timer 0.1 nil
                                   (lambda (fg) (set-face-background
                                                 'mode-line fg))
                                   orig-fg))))

;; (use-package ido
;;   :straight nil
;;   :bind
;;   ("C-x C-f" . ido-find-file)
;;   ("C-x b" . ido-switch-buffer)
;;   ("C-x C-d" . ido-dired)
;;   ;; ("C-x C-r" . ido-recentf-open)
;;   ;; :init
;;   ;; (defun ido-recentf-open ()
;;   ;;   "Use 'ido-completing-read' to \\[find-file] a recent file"
;;   ;;   (interactive)
;;   ;;   (if (find-file
;;   ;;        (ido-completing-read "Find recent file: " recentf-list))
;;   ;;       (message "Opening file...")
;;   ;;     (message "Aborting")))
;;   :config
;;   (setq ido-max-window-height 0.75)
;;   (setq ido-enable-flex-matching t)
;;   (setq ido-confirm-unique-completion t)
;;   (ido-mode 1)
;;   (ido-everywhere 1)
;;   (use-package ido-completing-read+
;;     :diminish
;;     :config
;;     (ido-ubiquitous-mode t))
;;   (use-package ido-vertical-mode
;;     :diminish
;;     :config
;;     (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
;;     (setq ido-vertical-show-count t)
;;     (setq ido-use-faces t)
;;     (ido-vertical-mode 1))
;;   (use-package flx-ido
;;     :diminish
;;     :config
;;     (flx-ido-mode 1))
;;   (use-package amx
;;     :bind
;;     ("M-x" . amx))
;;   (use-package ido-flex-with-migemo
;;     :defer t
;;     :diminish
;;     :hook
;;     (ido-mode-hook . ido-flex-with-migemo-mode)
;;     :config
;;     (add-to-list 'ido-flex-with-migemo-excluded-func-list
;;                  '(amx ido-switch-buffer))
;;     (setq ido-flex-with-migemo-least-char 5))
;;   (use-package ido-sort-mtime
;;     :config
;;     (ido-sort-mtime-mode 1)
;;     ;; (setq ido-sort-mtime-tramp-files-at-end nil)
;;     (setq ido-sort-mtime-dot-at-beginning t)
;;     (setq ido-sort-mtime-limit 2000))
;;   (use-package crm-custom
;;     :config
;;     (crm-custom-mode 1))
;;   (use-package ido-complete-space-or-hyphen
;;     :diminish))

;; (use-package helm
;;   :disabled t
;;   :diminish Helm
;;   :init
;;   (require 'helm-config)
;;   :bind
;;   (("M-x" . helm-M-x)
;;    ("C-x C-f" . helm-find-files)
;;    ("C-x C-r" . helm-recentf)
;;    ("C-x C-b" . helm-buffers-list)
;;    ("C-x b" . helm-mini)
;;    ("M-y" . helm-show-kill-ring)
;;    ("C-c SPC" . helm-all-mark-rings)
;;    ("C-c h g" . helm-google-suggest)
;;    (:map helm-map
;;          ("C-h" . delete-backward-char)
;;          ("<tab>" . helm-execute-persistent-action)
;;          ("C-z" . helm-select-action))
;;    (:map helm-find-files-map
;;          ("C-h" . delete-backward-char)))
;;   :config
;;   (global-set-key (kbd "C-c h") 'helm-command-prefix)
;;   (global-unset-key (kbd "C-x c"))
;;   (when (executable-find "curl")
;;     (setq helm-google-suggest-use-curl-p t))
;;   (setq helm-split-window-in-side-p t
;;         helm-move-line-cycle-in-source t
;;         helm-echo-input-in-header-line t
;;         helm-candidate-number-limit 100)
;;   (defun spacemacs//helm-hide-minibuffer-maybe ()
;;     "Hide minibuffer in Helm session if we use the header line as input field."
;;     (when (with-helm-buffer helm-echo-input-in-header-line)
;;       (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
;;         (overlay-put ov 'window (selected-window))
;;         (overlay-put ov 'face
;;                      (let ((bg-color (face-background 'default nil)))
;;                        `(:background ,bg-color :foreground ,bg-color)))
;;         (setq-local cursor-type nil))))
;;   (add-hook 'helm-minibuffer-set-up-hook
;;             'spacemacs//helm-hide-minibuffer-maybe)
;;   (setq helm-autoresize-max-height 0
;;         helm-autoresize-min-height 20)
;;   (helm-autoresize-mode 1)
;;   (setq helm-M-x-fuzzy-match t
;;         helm-buffers-fuzzy-matching t
;;         helm-recentf-fuzzy-match t
;;         helm-imenu-fuzzy-match t
;;         helm-apropos-fuzzy-match t
;;         helm-lisp-fuzzy-completion t)
;;   ;; (setq helm-surfraw-default-browser-function 'browse-url-generic
;;   ;;       browse-url-generic-program "google-chrome")
;;   (helm-mode 1))

;; (use-package helm-smex
;;   :defer t
;;   :diminish
;;   :bind
;;   ("M-x" . helm-smex)
;;   ("M-x" . helm-smex-major-mode-commands))

;; (use-package helm-fuzzy
;;   :init
;;   (with-eval-after-load 'helm
;;     (helm-fuzzy-mode 1))
;;   :config
;;   (setq helm-fuzzy-not-allow-fuzzy '("*helm-ag*")))

;; (use-package helm-swoop
;;   :disabled t
;;   :defer t
;;   :diminish
;;   :bind
;;   (("M-i" . helm-swoop)
;;   ("M-I" . helm-swoop-back-to-last-point)
;;   ("C-c M-i" . helm-multi-swoop)
;;   ("C-x M-i" . helm-multi-swoop-all)
;;   (:map helm-swoop-map
;;         ("M-i" . helm-multi-swoop-all-from-helm-swoop)
;;         ("M-m" . helm-multi-swoop-current-mode-from-helm-swoop)
;;         ("C-r" . helm-previous-line)
;;         ("C-s" . helm-next-line))
;;   (:map helm-multi-swoop-map
;;         ("C-r" . helm-previous-line)
;;         ("C-s" . helm-next-line)))
;;   :config
;;   (setq helm-swoop-split-with-multiple-windows t)
;;   (setq helm-swoop-split-direction 'split-window-vertically)
;;   (setq helm-swoop-move-to-line-cycle t)
;;   (setq helm-swoop-use-fuzzy-match t))

;; (use-package helm-ag
;;   :disabled t
;;   :defer t
;;   :diminish
;;   :bind
;;   ("C-M-g" . helm-ag)
;;   :config
;;   (setq helm-ag-base-command "rg -S --vimgrep --no-heading")
;;   (setq helm-ag-insert-at-point 'symbol))

;; (use-package helm-c-yasnippet
;;   :disabled t
;;   :defer t
;;   :diminish
;;   :bind
;;   ("C-c y" . helm-yas-complete)
;;   :config
;;   (setq helm-yas-space-match-any-greedy t))

;; (use-package helm-cider
;;   :disabled t
;;   :defer t
;;   :diminish
;;   :config
;;   (helm-cider-mode 1))

(use-package selectrum
  :init
  (selectrum-mode +1)
  :bind
  ("C-c z" . selectrum-repeat))
(use-package selectrum-prescient
  :diminish
  :demand t
  :after selectrum
  :config
  (selectrum-prescient-mode +1))

(use-package ctrlf
  :init
  (ctrlf-mode +1)
  :config
  (add-hook 'pdf-isearch-minor-mode-hook (lambda () (ctrlf-local-mode -1))))

(use-package browse-kill-ring
  :diminish
  :bind
  ("M-y" . browse-kill-ring))
(use-package easy-kill
  :diminish
  :bind
  ("M-w" . easy-kill)
  ("C-<SPC>" . easy-mark))

;; (use-package flx-isearch
;;   :disabled t
;;   :diminish
;;   :bind
;;   ("C-M-s" . flx-isearch-forward)
;;   ("C-M-r" . flx-isearch-backward))
;; (use-package isearch-dabbrev
;;   :disabled t
;;   :diminish
;;   :bind
;;   (:map isearch-mode-map
;;         ("<tab>" . isearch-dabbrev-expand)))
;; (use-package swoop
;;   :disabled t
;;   :defer t
;;   :bind
;;   ("C-o" . swoop)
;;   ("C-M-o" . swoop-multi)
;;   ("M-o" . swoop-pcre-regexp)
;;   ("C-S-o" . swoop-back-to-last-position)
;;   ("C-M-m" . swoop-migemo)
;;   :config
;;   (setq swoop-minibuffer-input-delay 0.4)
;;   (setq swoop-font-size: 0.8))

(use-package migemo
  :defer t
  :diminish
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (when (eq system-type 'drwin)
    (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict"))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix))

(use-package anzu
  :disabled t
  :defer t
  :diminish
  :config
  (global-anzu-mode +1)
  (custom-set-variables
   '(anzu-mode-lighter "")
   '(anzu-search-threshold 1000)))

(use-package ace-isearch
  :disabled t
  :diminish
  :config
  (global-ace-isearch-mode +1)
  (setq ace-isearch-jump-delay 0.5)
  (setq ace-isearch-function 'avy-goto-char)
  (setq ace-isearch-function-from-isearch 'swoop-from-isearch)
  (setq ace-isearch-use-function-from-isearch t)
  (setq ace-isearch-fallback-function 'swoop-from-isearch))

(use-package wgrep
  :defer t
  :diminish
  :config
  (use-package wgrep-ag :defer t :diminish))

(use-package ag
  :defer t
  :diminish
  :bind
  ("M-s a" . ag-project)
  :config
  (ag-highlight-search t)
  (ag-reuse-buffer t)
  (ag-reuse-window t))

(use-package projectile
  :disabled t
  :defer t
  :diminish proj
  :bind
  ("s-p" . projectile-command-map)
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1))

(use-package visual-regexp
  :defer t
  :diminish
  :bind
  ("C-c r" . vr/replace)
  ("M-%" . vr/query-replace)
  ("C-M-S" . vr/isearch-forward)
  ("C-M-R" . vr/isearch-backward)
  ("C-c m" . vr/mc-mark)
  :config
  (use-package visual-regexp-steroids
    :diminish
    :config
    (setq vr/engine 'pcre2el)))       ; If use Python, pcre2el -> python

(use-package multiple-cursors
  :defer t
  :diminish
  :bind
  (("C-S-l" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this)
  ("M-S-<mouse-1>" . mc/add-cursor-on-click)
  :map mc/keymap
  ("M-<down>" . mc/cycle-forward)
  ("M-<up>" . mc/cycle-backward)
  ("M-S-<down>" . mc/skip-to-next-like-this)
  ("M-S-<up>" . mc/skip-to-previous-like-this))
  :init
  (progn
    (require 'mc-cycle-cursors)

    (defvar mc-last-direction 0
      "Records the last direction of multiple cursor.")

    (defun jump-to-next-cursor (another)
      (call-interactively
       (if (= mc-last-direction -1)
           'mc/cycle-backward
         'mc/cycle-forward))
      (setq mc-last-direction 1))

    (defun jump-to-previous-cursor (another)
      (call-interactively
       (if (= mc-last-direction 1)
           'mc/cycle-forward
         'mc/cycle-backward))
      (setq mc-last-direction -1))

    (defun reset-cursors (another)
      (setq mc-last-direction 0))

    (advice-add 'mc/mark-next-like-this
                :after 'jump-to-next-cursor)
    (advice-add 'mc/mark-previous-like-this
                :after 'jump-to-previous-cursor)
    (advice-add 'multiple-cursors-mode
                :after 'reset-cursors)))

(use-package expand-region
  :defer t
  :diminish
  :bind
  ("C-=" . er/expand-region))

(use-package avy
  :defer t
  :diminish
  :bind
  ("C-c C-j" . avy-resume)
  ("C-:" . avy-goto-char)
  ("C-'" . avy-goto-char-2)
  ("M-g f" . avy-goto-line)
  ("M-g w" . avy-goto-word-1)
  ("M-g e" . avy-goto-word-0)
  :config
  (avy-setup-default))
(use-package avy-migemo
  :defer t
  :diminish
  :bind
  ("M-g m m" . avy-migemo-mode)
  :config
  (avy-migemo-mode 1))

(use-package zzz-to-char
  :defer t
  :diminish
  :bind
  ("M-z" . zzz-up-to-char))

(use-package ace-window
  :defer t
  :diminish
  :bind
  ("s-w" . ace-window)
  :config
  (setq aw-keys '(?j ?k ?l ?i ?o ?h ?y ?u ?p))
  (setq aw-leading-char-face '((t (:height 4.0 :foreground "#f1fa8c")))))

(use-package mwim
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

(use-package beginend
  :defer t
  :diminish
  :config
  (beginend-global-mode))

(use-package visible-mark
  :defer t
  :diminish
  :config
  (setq set-mark-command-repeat-pop t)
  (setq visible-mark-max 10)
  (global-visible-mark-mode 1))

(use-package move-text
  :defer t
  :diminish
  :config
  (move-text-default-bindings))

(use-package company
  :defer t
  :bind
  (("C-M-i" . company-complete)
   (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("<tab>" . company-complete-common-or-cycle))
   (:map company-search-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)))
  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-minimum-prefix-length 1)
  (company-show-numbers t)
  :hook
  (after-init . global-company-mode)
  :config
  (setq company-selection-wrap-around t)
  ;; (add-to-list 'company-backends #'company-tabnine)
  ;; (add-to-list 'company-backends ')
  (use-package company-box
    :diminish
    :hook
    (company-mode-hook . company-box-mode))
  (use-package company-posframe
    :diminish
    :hook
    (company-mode-hook . company-posframe-mode))
  ;; (use-package company-tabnine :diminish)
  (use-package company-quickhelp
    :when (display-graphic-p)
    :diminish
    :bind
    (:map company-active-map
          ("M-h" . company-quickhelp-manual-begin))
    :hook
    (global-company-mode-hook . company-quickhelp-mode)
    :custom
    (company-quickhelp-delay 0.8))
  (use-package company-auctex
    :diminish
    :defer t
    :config
    (company-auctex-init))
  (use-package company-math
    :diminish
    :defer t
    :preface
    (defun my/latex-mode-setup ()
      (setq-local company-backends
                  (append '((company-math-symbols-latex
                             company-latex-commands
                             company-math-symbols-unicode))
                          company-backends)))
    :hook
    ((org-mode-hook . my/latex-mode-setup)
     (TeX-mode-hook . my/latex-mode-setup)))
  ;; (use-package company-math
  ;;   :diminish
  ;;   :defer t
  ;;   :preface
  ;;   (defun c/latex-mode-setup ()
  ;;     (setq-local company-backends
  ;;                 (append '((company-math-symbols-latex
  ;;                            company-math-symbols-unicode
  ;;                            company-latex-commands))
  ;;                         company-backends)))
  ;;   :hook
  ;;   ((org-mode-hook . c/latex-mode-setup)
  ;;    (tex-mode-hook . c/latex-mode-setup)))
  )

(use-package hippie
  :straight nil
  :bind
  ("C--" . hippie-expand)
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

;; (use-package bbyac
;;   :bind
;;   (:map bbyac-mode-map
;;         ("C-@" . bbyac-expand-symbols))
;;   :config
;;   (setq bbyac-max-chars 99999)
;;   (defun bbyac--display-matches--use-ido (orig strlist)
;;     (cond ((null (cdr strlist))
;;            (car strlist))
;;           ((cl-notany #'bbyac--string-multiline-p strlist)
;;            (ido-completing-read "Bbyac: " strlist nil t))
;;           (t (apply orig strlist))))
;;   (advice-add 'bbyac--display-matches :around 'bbyac--display-matches--use-ido)
;;   (bbyac-global-mode 1))

(use-package yasnippet
  :defer t
  :diminish
  :bind
  ("C-c s i" . yas-insert-snippet)
  ("C-c s n" . yas-new-snippet)
  ("C-c s v" . yas-visit-snippet-file)
  ("C-c s l" . yas-describe-tables)
  ("C-c s g" . yas-reload-all)
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"
          "~/.emacs.d/mysnippets"))
  (yas-global-mode 1))
(use-package yasnippet-snippets
  :after yasnippet)

(use-package org
  :straight nil
  :hook
  (org-mode-hook . org-indent-mode)
  :config
  (setq org-tags-column 0)
  (setq org-display-inline-images t)
  (setq org-redisplay-inline-images t)
  (setq org-startup-with-inline-images "inlineimages")
  (setq org-hide-emphasis-markers t)
  (setq org-confirm-elisp-link-function nil)
  (setq org-link-frame-setup '((file . find-file)))
  ;; (setq-ligatures! 'org-mode
  ;;                  :alist '(("TODO " . "")
  ;;                           ("NEXT " . "")
  ;;                           ("PROG " . "")
  ;;                           ("WAIT " . "")
  ;;                           ("DONE " . "")
  ;;                           ("FAIL " . "")))
  (setq org-ellipsis "⋯"))

(use-package org-superstar
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

;; Org-Babel tangle
(require 'ob-tangle)
;; Setup Babel languages. Can now do Literate Programming
(org-babel-do-load-languages 'org-babel-load-languages
                             '((python . t)
                               (shell . t)
	                           (emacs-lisp . t)
	                           (ledger . t)
	                           (ditaa . t)
	                           (js . t)
	                           (C . t)))

;  (use-package recursive-narrow :defer t :diminish)

(use-package auctex
  :defer t
  :hook
  (LaTeX-mode-hook . (turn-on-reftex
                      LaTeX-math-mode
                      outline-minor-mode))
  :mode
  (("\\.tex\\'" . LaTeX-mode)
   ("\\.sty\\'" . LaTeX-mode)
   ("\\.bib\\'" . LaTeX-mode)
   ("\\.cls\\'" . LaTeX-mode))
  :config
  (setq-default TeX-master nil
                TeX-PDF-mode t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-close-quote "")
  (setq TeX-open-quote "")
  (setq LaTeX-electric-left-right-brace t)
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-format-cite-function 
        '(lambda (key fmt)
	       (let ((cite (replace-regexp-in-string "%l" key fmt)))
	         (if (or (= ?~ (string-to-char fmt))
		            (member (preceding-char) '(?\ ?\t ?\n ?~)))
	             cite
	           (concat "~" cite))))))
(use-package cdlatex
  :defer t
  :diminish
  :hook
  (LaTeX-mode-hook . turn-on-cdlatex)
  (org-mode-hook . turn-on-org-cdlatex))

;; (use-package slime
;;   :defer t
;;   :if (file-exists-p "~/.roswell/helper.el")
;;   :ensure slime-company
;;   :init (load "~/.roswell/helper.el")
;;   :custom (inferior-lisp-program "ros -Q run")
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.lisp$" . lisp-mode))
;;   (setq slime-net-coding-system 'utf-8-unix)
;;   (eval-after-load "slime"
;;     '(slime-setup '(slime-fancy slime-banner slime-company))))
(use-package sly
  :disabled t
  :defer t
  :if (file-exists-p "~/.roswell/helper.el")
  :init (load "~/.roswell/helper.el")
  :custom (inferior-lisp-program "ros -Q run")
  :config
  (add-to-list 'auto-mode-alist '("\\.lisp$" . lisp-mode)))
(use-package cl-lib :diminish)

(use-package racket-mode
  :disabled t
  :defer t
  :diminish
  :bind
  (:map racket-mode-map
        ("<f5>" . racket-run))
  :config
(setq tab-always-indent 'complete)
(setq font-lock-maximum-decoration 3))

;; (use-package cider
;;   :defer t
;;   :diminish
;;   :)

(use-package elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq elpy-rpc-virtualenv-path 'current)
  (setq elpy-rpc-backend "jedi")
  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)))

;; (use-package markdown-mode)

(use-package quickrun
  :defer t
  :diminish)

(use-package dumb-jump
  :disabled t)

(use-package ispell
  :straight nil
  :defer t
  :if
  (file-executable-p "aspell")
  :custom
  (ispell-program-name "aspell")
  :config
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

(use-package flyspell
  :defer t
  :diminish
  :if (executable-find "aspell")
  :bind
  ("<f12>" . flyspell-mode)
  ("<f10>" . flyspell-buffer)
  :hook
  ((prog-mode . flyspell-prog-mode)
   (TeX-mode . flyspell-mode)
   (org-mode . flyspell-mode)
   (text-mode . flyspell-mode))
  :config
  )

(use-package typo
  :disabled t
  :defer t
  :diminish
  :bind
  ("C-c 8")
  :config
  (typo-global-mode 1))

(use-package eshell
  :straight nil
  :init
  (setq eshell-scroll-to-bottom-on-input 'all
        eshell-error-if-no-glob t
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t
        eshell-prefer-lisp-functions nil)
  :config
  (setq eshell-ask-to-save-history (quote always))
  (setq eshell-cmpl-cycle-completions nil)
  (setq eshell-cmpl-ignore-case t)
  (setq eshell-prompt-regexp "^[^#$]*[$#] ")
  (setq eshell-command-aliases-list
    (append
     (list
      (list "ll" "ls -ltr")
      (list "la" "ls -a")
      (list "o" "xdg-open")
      ;; (list "emacs" "find-file $1")
      (list "m" "find-file $1")
      (list "mc" "find-file $1")
      (list "l" "eshell/less $1")
      (list "d" "dired .")
      (list "ff" "find-file $1")
      (list "FF" "find-file-other-window $1")
      (list "v" "view-file $1")
      (list "V" "view-file-other-window $1")
      (list "up" "eshell-up $1")
      (list "pk" "eshell-up-peek $1")
      )))
  (use-package eshell-z :diminish)
  (use-package eshell-prompt-extras
    :diminish
    :defer t
    :config
    (setq eshell-highlight-prompt t
          eshell-prompt-function 'epe-theme-lambda))
  (use-package eshell-did-you-mean
    :diminish
    :defer t
    :config
    (eshell-did-you-mean-setup))
  (use-package eshell-up
    :diminish
    :defer t)
  (use-package esh-autosuggest
    :diminish
    :defer t
    :hook (eshell-mode . esh-autosuggest-mode)))

(use-package shell-pop
  :defer t
  :bind
  ("C-c C-s" . shell-pop)
  :config
  (custom-set-variables
   '(shell-pop-shell-type '("eshell" "*eshell*"
                               (lambda ()
                                 (eshell))))
   '(shell-pop-term-shell "/usr/local/bin/zsh")
   '(shell-pop-universal-key "C-t")
   '(shell-pop-default-directory "/Users/iwata")
   '(shell-pop-autocd-to-working-dir t)
   '(shell-pop-full-span t)
   '(shell-pop-cleanup-buffer-at-process-exit t)
   '(shell-pop-restore-window-configuration t)
   '(shell-pop-window-height 30)
   '(shell-pop-window-position "bottom")))

(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(set-face-attribute 'default nil
		    :family "Source Code Pro"
		    :height 125)
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Noto Sans CJK JP"))
(add-to-list 'face-font-rescale-alist '(".*Noto.*" . 1.2))

;; Test text from https://qiita.com/kaz-yos/items/0f23d53256c2a3bd6b8d
;; |012345 678910|
;; |abcdef ghijkl|
;; |ABCDEF GHIJKL|
;; |αβγδεζ ηθικλμ|
;; |ΑΒΓΔΕΖ ΗΘΙΚΛΜ|
;; |∩∪∞≤≥∏ ∑∫×±⊆⊇|
;; |'";:-+ =/\~`?|
;; |日本語 の美観|
;; |あいう えおか|
;; |アイウ エオカ|
;; |ｱｲｳｴｵｶ ｷｸｹｺｻｼ|
;;
;; | hoge                 | hogehoge | age               |
;; |----------------------+----------+-------------------|
;; | 今日もいい天気ですね | お、     | 等幅になった 👍 |
;; | 🎙マイクで🌈虹が出る | お、     | 等幅になった 👍 |;; Test text from https://qiita.com/kaz-yos/items/0f23d53256c2a3bd6b8d
;; |012345 678910|
;; |abcdef ghijkl|
;; |ABCDEF GHIJKL|
;; |αβγδεζ ηθικλμ|
;; |ΑΒΓΔΕΖ ΗΘΙΚΛΜ|
;; |∩∪∞≤≥∏ ∑∫×±⊆⊇|
;; |'";:-+ =/\~`?|
;; |日本語 の美観|
;; |あいう えおか|
;; |アイウ エオカ|
;; |ｱｲｳｴｵｶ ｷｸｹｺｻｼ|
;;
;; | hoge                 | hogehoge | age               |
;; |----------------------+----------+-------------------|
;; | 今日もいい天気ですね | お、     | 等幅になった 👍 |
;; | 🎙マイクで🌈虹が出る | お、     | 等幅になった 👍 |

;; all-the-icons
(use-package all-the-icons)
;; pretty-mode
(use-package pretty-mode
  :diminish
  :config
  (global-pretty-mode t))

;; (use-package doom-themes
;;   :disabled t
;;   :config
;;   (setq doom-themes-enable-bold t
;;         doom-themes-enable-italic t)
;;   (load-theme 'doom-acario-light t)
;;   ;; (load-theme 'doom-palenight t)
;;   (doom-themes-visual-bell-config)
;;   (doom-themes-org-config)
;;   (setq doom-themes-treemacs-theme "doom-colors") ;use the colorful treemacs theme
;;   (doom-themes-treemacs-config))

;; (use-package acme-theme
;;   :disabled t
;;   :straight (acme-theme :host github
;;                         :repo "ianpan870102/acme-emacs-theme"
;;                         :branch "master")
;;   :config
;;   (load-theme 'acme t)
;;   (setq acme-theme-black-fg t))

(use-package berrys-theme
  :disabled t
  :config
  (load-theme 'berrys t)
  :config ;; for good measure and clarity
  (setq-default cursor-type '(bar . 2))
  (setq-default line-spacing 2))

(use-package apropospriate-theme)

;; (use-package zerodark-theme
;;   ;; Dark Theme
;;   :defer t)
;; (use-package nord-theme
;;   :defer t)

(use-package circadian
  :defer t
  :config
  (add-hook 'circadian-after-load-theme-hook
        #'(lambda (theme)
            ;; Line numbers appearance
            (setq linum-format 'linum-format-func)
            ;; Cursor
            (set-default 'cursor-type 'box)
            (set-cursor-color "#00FFFF")))
  (setq calendar-latitude 34.887760)
  (setq calendar-longitude 135.799850)

  ;; (setq circadian-themes '(("8:00" . apropospriate-light)
  ;;                          ("19:30" . apropospriate-dark)))

  (setq circadian-themes '((:sunrise . apropospriate-light)
                           (:sunset . apropospriate-dark)))
  (circadian-setup))

(display-time-mode 1)
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-battery-mode t)
(column-number-mode 1)

(use-package doom-modeline
  :disabled t
  :diminish
  :init
  (doom-modeline-mode 1))

(use-package awesome-tray
  :straight (awesome-tray :host github
                          :repo "manateelazycat/awesome-tray"
                          :branch "master")
  :diminish
  :init
  (awesome-tray-mode 1))

(use-package dashboard
  :diminish dashboard-mode
  :custom
  (dashboard-startup-banner '"~/.emacs.d/image/Larry_Cow.png")
  ;; Value can be
  ;; 'official which displays the official emacs logo
  ;; 'logo which displays an alternative emacs logo
  ;; 1, 2 or 3 which displays one of the text banners
  ;; "path/to/your/image.png" which displays whatever image you would prefer
  (dashboard-center-content t)
  (dashboard-items '((recents .5)))
      ;               (projects .5)
       ;              (bookmarks .5)))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-navigator t)
  (dashboard-set-init-info t)
  :hook
  (after-init . dashboard-setup-startup-hook)
  :config
  (setq initial-buffer-choice
        (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-navigator-buttons
        `(;; line1
          ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0) 
            "GitHub" 
        "Browse GitHub"
        (lambda (&rest _) (browse-url "https://github.com/aki-pooh1244")))))))

;; Transparency

(add-to-list 'default-frame-alist
	     '(alpha . (0.90 0.90)))

(use-package multicolumn
  :defer t
  :diminish
  :init
  (multicolumn-global-mode 1)
  :config
  (setq multicolumn-min-width 72))

(use-package smooth-scroll
  :config
  (smooth-scroll-mode t))

;; Windowmove

(use-package windmove
  :straight nil
  :config
  (windmove-default-keybindings 'super))

(use-package elscreen
  :defer t
  :diminish els
  :bind
  ("C-<tab>" . elscreen-next)
  :config
  (setq elscreen-prefix-key (kbd "C-x q"))
  (setq elscreen-display-tab nil)
  (setq elscreen-tab-display-kill-screen nil)
  (setq elscreen-tab-display-control nil)
  (elscreen-start)
  (elscreen-create)
  (use-package elscreen-separate-buffer-list
    :diminish
    :defer t
    :config
    (elscreen-separate-buffer-list-mode 1))
  (use-package zoom-window
    :defer t
    :diminish
    :bind
    ("C-x z" . zoom-window-zoom)
    :config
    (setq zoom-window-use-elscreen t)
    (zoom-window-setup)))

(use-package transpose-frame)

(use-package rainbow-mode
  :disabled t
  :diminish
  :hook ((emacs-lisp-mode c-mode org-mode) . rainbow-mode))
(use-package rainbow-delimiters
  :diminish
  :hook
  (emacs-lisp-mode-hook. rainbow-delimiters-mode)
  (prog-mode-hook . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-outermost-only-face-count 1)
  (rainbow-delimiters-mode 1)
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#9a4040")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#ff5e5e")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#ffaa77")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#dddd77")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#80ee80")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#66bbff")
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#da6bda")
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#afafaf")
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#f0f0f0"))

;; Polymode
(use-package polymode
  :diminish
  :config
  (use-package poly-org)
  (use-package poly-markdown)
  (add-to-list 'auto-mode-alist
               '("\\.md" . poly-markdown-mode)
               '("\\.org" . poly-org-mode)))
(use-package highlight-stages
  :defer t
  :diminish
  :config
  (highlight-stages-global-mode 1))

(use-package prism
  :defer t
  :diminish
  :hook
  (elisp-mode-hook . prism-mode))

(use-package beacon
  :defer t
  :diminish
  :config
  (beacon-mode 1))

(use-package volatile-highlights
  :defer t
  :diminish
  :config
  (volatile-highlights-mode t))

(use-package posframe :diminish)

(use-package mini-frame
  :disabled t
  :diminish
  :hook
  (after-init . mini-frame-mode)
  :config
  (custom-set-variables
   '(mini-frame-show-parameters
     '((top . 0)
       (width . 1.0)
       (left . 0.5)
       (height . 20)))))

(use-package popwin :diminish)

(use-package symon
  :defer t
  :diminish
  :config
  (when (eq system-type 'darwin)
    (setq symon-monitors
        '(symon-current-time-monitor
          symon-darwin-cpu-monitor
          symon-darwin-memory-monitor
          symon-darwin-battery-monitor
          symon-darwin-network-rx-monitor
          symon-darwin-network-tx-monitor)))
  (when (eq system-type 'linux)
    (setq symon-monitors
        '(symon-current-time-monitor     
          symon-linux-cpu-monitor        
          symon-linux-memory-monitor     
          symon-linux-battery-monitor    
          symon-linux-network-rx-monitor 
          symon-linux-network-tx-monitor)))

  (setq symon-sparkline-type 'plain)
  (setq symon-sparkline-height 10)
  (setq symon-sparkline-width 50)
  (setq symon-sparkline-thickness 2)
  (symon-mode 1))

(use-package git-gutter
  :disabled t
  :defer t
  :diminish
  :config
  (global-git-gutter-mode +1)
  (custom-set-variables
   '(git-gutter:update-interval 2))
  (custom-set-variables
   '(git-gutter:modified-sign "  ")
   '(git-gutter:added-sign "++")
   '(git-gutter:deleted-sign "--"))
  (set-face-background 'git-gutter:modified "purple")
  (set-face-background 'git-gutter:added "green")
  (set-face-background 'git-gutter:deleted "red")
  (custom-set-variables
   '(git-gutter:separator-sign "|"))
  (set-face-foreground 'git-gutter:separator "yellow")    
  (custom-set-variables
   '(git-gutter:hide-gutter t)))
(use-package diff-hl
  :defer t
  :diminish
  :init
  (global-diff-hl-mode))

(use-package pdf-tools
  :config
  (pdf-tools-install)
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (setq pdf-view-resize-factor 1.1))

(use-package olivetti
  :defer t
  :bind
  ("<f7>" . olivetti-mode)
  :init
  (setq olivetti-body-width 0.618))

(use-package google-this
  :defer t
  :bind
  ("C-x g" . google-this-mode-submap)
  :config
  (google-this-mode 1)
  (setq google-this-location-suffix "co.jp"))
(use-package google-translate
  :defer t
  :bind
  ("C-c g t" . google-translate-at-point)
  ("C-c g T" . google-translate-at-point-reverse)
  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "jp"))
