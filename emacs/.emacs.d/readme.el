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
  :init
  :config
  (exec-path-from-shell-initialize))

(use-package f)
(use-package s)
(use-package dash)

(setq-default cursor-type 'box)
(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode +1)
(delete-selection-mode t)
(size-indication-mode t)
(display-line-numbers-mode -1)
(setq scroll-conservatively 100000
      scroll-margin 0
      scroll-step 1
  scroll-preserve-screen-position 1)

;; (setq frame-title-format
;;   (format "%%f - Emacs@%s" (system-name)))

;; (setq frame-title-format
;;     '((:eval (if (buffer-file-name)
;;                  (abbreviation-file-name (buffer-file-name))
;;                "%b"))))

(setq frame-title-format
      '("emacs%@"
        (:eval (system-name)) ": "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b")) " [%*]"))

(use-package uniquify
  :straight nil
  :delight
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(setq show-paren-style 'parethesis)
(show-paren-mode +1)

(electric-pair-mode +1)

(fset 'yes-or-no-p 'y-or-n-p)

(setq ring-bell-function 'ignore)
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)
;; (global-whitespace-mode +1)
(setq-default tab-width 4 indent-tabs-mode nil)
(setq default-tab-width 4)
(setq vc-follow-symlinks t)
(setq tab-always-indent 'complete)
(setq blink-matching-paren nil)
(setq require-final-newline t)
(global-auto-revert-mode t)

;; calender style = English
(setq system-time-locale "C")

(use-package crux
  :defer t
  :delight
  :bind
  ("C-c o" . crux-open-with)
  ("C-k" . crux-smart-kill-line)
  ("C-s-RET" . crux-smart-open-line)
  ("s-RET" . crux-smart-pen-line)
  ("s-j" . crux-top-join-line)
  ("C-<backspace>" . crux-kill-line-backwards)
  ("C-c d" . crux-duplicate-current-line-or-region)
  ("s-r" . crux-recentf-ido-find-file)
  ("C-c ," . crux-find-user-custom-file)
  ("C-c e" . crux-eval-and-replace)
  :config
  (crux-with-region-or-buffer indent-region))

(use-package smart-newline
  :disabled t
  :defer t
  :delight
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

(use-package ibuffer
  :straight nil
  :bind
  ("C-x C-b" . ibuffer-bs-show))

(use-package persistent-scratch
  :defer t
  :delight
  :config
  (persistent-scratch-setup-default))

(use-package electric-operator
  :delight
  :hook
  (c-mode-hook . electric-operator-mode)
  (c++-mode-hook . electric-operator-mode)
  (python-mode-hook . electric-operator-mode)
  (perl-mode-hook . electric-operator-mode))

(use-package aggressive-indent
  :defer t
  :delight
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

(use-package async
  :delight
  :hook after-init
  :config
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1))

(use-package delight
  :defer t)

(use-package posframe :delight)

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
  (setq ns-option-modifier (quote super))
  ;; capslock -> hyper
  )

(use-package which-key
  :delight
  :hook (after-init . which-key-mode)
  :config
  (use-package which-key-posframe
    :hook (which-key-mode . which-key-posframe-mode)))

(use-package key-chord
  :delight
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
  (key-chord-define-global "ff" 'dired-sidebar-toggle-sidebar)
  (key-chord-mode +1)
  (use-package use-package-chords))

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
  :delight
  :config
  (prescient-persist-mode +1)
  (setq prescient-history-length 10000))

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
;; (use-package recentf-ext
;;   :defer t
;;   :delight
;;   :bind
;;   ("C-c c o" . recentf-open-files))

(use-package frecentf
  :delight Recentf
  :defer t
  :hook
  (after-init . frecentf-mode))

(use-package super-save
  :delight
  ;; :hook
  ;; (after-init-hook . super-save-mode)
  :config
  (add-to-list 'super-save-triggers 'ace-window)
  (add-to-list 'super-save-triggers 'find-file-hook)
  (setq super-save-auto-save-when-idle t)
  (setq super-save-remote-files nil)
  (super-save-mode +1))
(use-package save-place
  :straight nil
  :delight
  :config
  (save-place-mode 1))
;; (use-package savehist-mode
;;   :straight nil
;;   :delight
;;   :config
;;   (push 'kill-ring savehist-additional-variables)
;;   (push 'command-history savehist-ignored-variables)
;;   (savehist-mode 1))
(use-package psession
  :config
  (psession-mode 1)
  (psession-savehist-mode 1)
  (psession-autosave-mode 1))

(use-package undo-fu
  :delight
  :bind
  ("C-/" . undo-fu-only-undo)
  ("M-/" . undo-fu-only-redo))
(use-package undo-fu-session
  :delight
  :config
  (setq undo-fu-session-compression t)
  (global-undo-fu-session-mode))

(use-package dired
  :straight nil
  :delight Dir
  :hook
  (dired-mode . auto-revert-mode)
  :bind
  ((:map dired-mode-map
	 ("(" . dired-hide-details-mode)
	 (")" . dired-hide-details-mode)))
  :custom
  (dired-listing-switches "-alh")
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-isearch-filenames 'dwim)
  (dired-ls-F-marks-symlinks t)
  :config
  ;; (add-to-list 'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip"))
  (use-package wdired
    :straight nil
    :delight Wdir
    :demand dired
    :after dired
    :bind
    ((:map dired-mode-map
	   ("e" . wdired-change-to-wdired-mode)))
    :custom
    (wdired-allow-to-change-permissions t))
  (use-package dired-aux
    :straight nil
    :after dired)
  (use-package all-the-icons-dired
    :delight
    :after all-the-icons
    :hook
    (dired-mode . all-the-icons-dired-mode))
  ;; dired-hack packages
  (use-package dired-hacks-utils
    :delight
    :after dired)
  (use-package dired-filter
    :delight Fil
    :after dired
    :bind
    ((:map dired-mode-map
	       ("/" . dired-filter-map))))
  (use-package dired-narrow
    :disabled t
    :delight Nar
    :after dired
    :bind
    ((:map dired-mode-map
           ("f" . dired-narrow-fuzzy))))
  (use-package dired-rainbow
    :config
    (progn
	  (dired-rainbow-define-chmod directory
				                  "#6cb2eb" "d.*")
	  (dired-rainbow-define html
			                "#eb5286"
			                ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
	  (dired-rainbow-define xml
			                "#f2d024"
			                ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
	  (dired-rainbow-define document
			                "#9561e2"
			                ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
	  (dired-rainbow-define markdown
			                "#ffed4a"
			                ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
	  (dired-rainbow-define database
			                "#6574cd"
			                ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
	  (dired-rainbow-define media
			                "#de751f"
			                ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
	  (dired-rainbow-define image
			                "#f66d9b"
			                ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
	  (dired-rainbow-define log
			                "#c17d11"
			                ("log"))
	  (dired-rainbow-define shell
			                "#f6993f"
			                ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
	  (dired-rainbow-define interpreted
			                "#38c172"
			                ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
	  (dired-rainbow-define compiled
			                "#4dc0b5"
			                ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
	  (dired-rainbow-define executable
			                "#8cc4ff"
			                ("exe" "msi"))
	  (dired-rainbow-define compressed
			                "#51d88a"
			                ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
	  (dired-rainbow-define packaged
			                "#faad63"
			                ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
	  (dired-rainbow-define encrypted
			                "#ffed4a"
			                ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
	  (dired-rainbow-define fonts
			                "#6cb2eb"
			                ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
	  (dired-rainbow-define partition
			                "#e3342f"
			                ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
	  (dired-rainbow-define vc
			                "#0074d9"
			                ("git" "gitignore" "gitattributes" "gitmodules"))
	  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")))
  (use-package dired-collapse
    :delight
    :after dired)
  (use-package dired-avfs
    :delight
    :after dired)
  (use-package dired-subtree
    :delight
    :after dired
    :bind
    ((:map dired-mode-map
	   ("i" . dired-subtree-toggle))))
  ;; dired-hack packages end here
  (use-package peep-dired
    :delight peep
    :defer t
    :bind
    ((:map dired-mode-map
	   ("g" . peep-dired))))
  (use-package quick-preview
    :delight preview
    :init
    :bind
    ("C-c q" . quick-preview-at-point)
    (:map dired-mode-map
	      ("Q" . quick-preview-at-point)))
  (use-package runner
    :delight
    :after dired)
  (use-package dired-toggle-sudo
    :delight sudo
    :after dired
    :bind
    ((:map dired-mode-map
	   ("C-c C-s" . dired-toggle-sudo)))
    :config
    (with-eval-after-load 'tramp
      ;; Allow to use : /sudo:user@host:/path/to/file
      (add-to-list 'tramp-default-proxies-alist
		   '(".*" "\\'.+\\" "/ssh:%h:"))))
  )

(use-package dired-sidebar
  :defer t
  :delight
  :bind ("C-c C-n" . dired-sidebar-toggle-sidebar)
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
	    (lambda ()
	      (unless (file-remote-p default-directory)
		(auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'icons)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

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

(use-package selectrum
  :init
  (selectrum-mode +1)
  :bind
  ("C-c z" . selectrum-repeat))
(use-package selectrum-prescient
  :delight
  :demand t
  :after selectrum
  :config
  (selectrum-prescient-mode +1))

(use-package loccur
  :delight loccur
  )

(use-package easy-kill
  :delight
  :bind
  ("M-w" . easy-kill)
  ("C-<SPC>" . easy-mark))

(use-package browse-kill-ring
  :delight
  :bind
  ("M-y" . browse-kill-ring))

(use-package migemo
  ;; :defer t
  ;; :delight
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (when (eq system-type 'drwin)
    (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict"))
  ;; (when (eq system-type 'gnu/linux)
  ;;   (setq migemo-dictionary "/usr/..."))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (migemo-init))

(use-package wgrep
  :disabled t
  :defer t
  :delight
  )

(use-package deadgrep
  :defer t
  :delight deadgrep
  :bind
  ("<f5>" . deadgrep))

(use-package projectile
  :disabled t
  :defer t
  :delight proj
  :bind
  ("s-p" . projectile-command-map)
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1))

(use-package visual-regexp
  :defer t
  :delight
  :bind
  ("C-c r" . vr/replace)
  ("M-%" . vr/query-replace)
  ("C-M-S" . vr/isearch-forward)
  ("C-M-R" . vr/isearch-backward)
  ("C-c m" . vr/mc-mark)
  :config
  (use-package pcre2el)
  (use-package visual-regexp-steroids
    :after visual-regexp
    :delight
    :config
    (setq vr/engine 'pcre2el)))       ; If use Python, pcre2el -> python

(use-package anzu
  :delight anzu
  :bind
  ("C-M-%" . anzu-query-replace)
  :config
  (global-anzu-mode +1)
  (setq anzu-use-migemo t)
  (setq anzu-search-threshold 100)
  (setq anzu-replace-to-string-separator " => "))

;; (use-package ctrlf
;;   :init
;;   (ctrlf-mode +1)
;;   :config
;;   (add-hook 'pdf-isearch-minor-mode-hook (lambda () (ctrlf-local-mode -1))))

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
;;     :delight
;;     :config
;;     (ido-ubiquitous-mode t))
;;   (use-package ido-vertical-mode
;;     :delight
;;     :config
;;     (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
;;     (setq ido-vertical-show-count t)
;;     (setq ido-use-faces t)
;;     (ido-vertical-mode 1))
;;   (use-package flx-ido
;;     :delight
;;     :config
;;     (flx-ido-mode 1))
;;   (use-package amx
;;     :bind
;;     ("M-x" . amx))
;;   (use-package ido-flex-with-migemo
;;     :defer t
;;     :delight
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
;;     :delight))

;; (use-package helm  
;;   :disabled t
;;   :delight Helm
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
;;   :delight
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
;;   :delight
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
;;   :delight
;;   :bind
;;   ("C-M-g" . helm-ag)
;;   :config
;;   (setq helm-ag-base-command "rg -S --vimgrep --no-heading")
;;   (setq helm-ag-insert-at-point 'symbol))

;; (use-package helm-c-yasnippet
;;   :disabled t
;;   :defer t
;;   :delight
;;   :bind
;;   ("C-c y" . helm-yas-complete)
;;   :config
;;   (setq helm-yas-space-match-any-greedy t))

;; (use-package helm-cider
;;   :disabled t
;;   :defer t
;;   :delight
;;   :config
;;   (helm-cider-mode 1))

;; (use-package flx-isearch
;;   :disabled t
;;   :delight
;;   :bind
;;   ("C-M-s" . flx-isearch-forward)
;;   ("C-M-r" . flx-isearch-backward))
;; (use-package isearch-dabbrev
;;   :disabled t
;;   :delight
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

;; (use-package ace-isearch
;;   :disabled t
;;   :delight
;;   :config
;;   (global-ace-isearch-mode +1)
;;   (setq ace-isearch-jump-delay 0.5)
;;   (setq ace-isearch-function 'avy-goto-char)
;;   (setq ace-isearch-function-from-isearch 'swoop-from-isearch)
;;   (setq ace-isearch-use-function-from-isearch t)
;;   (setq ace-isearch-fallback-function 'swoop-from-isearch))

(use-package counsel
  :disabled t
  :delight Ivy Counsel
  :init (ivy-mode 1)
  :bind
  (("C-s" . swiper)
   ("M-x" . counsel-M-x)
   ("M-y" . counsel-yank-pop)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-r" . counsel-recentf)
   ("C-x b" . counsel-switch-buffer)
   :map ivy-minibuffer-map
   ("C-k" . ivy-kill-line)
   ("C-j" . ivy-immediate-done)
   ("RET" . ivy-alt-done)
   ("C-h" . ivy-backward-char))
  :hook
  (ivy-mode . counsel-mode)
  :custom
  (counsel-yank-pop-height 20)
  (enable-recursive-minibuffers t)
  (ivy-height 20)
  (ivy-use-selectable-prompt t)
  (ivy-format-functions-alist '((t . ivy-format-function-arrow)))
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-re-builders-alist '((t. ivy--regex-fuzzy)
                           (swiper . ivy--regex-plus)))
  (counsel-yank-pop-separator "\n------------\n")
  :config
  (use-package ivy-prescient
    :delight
    :demand t
    :after ivy perscient
    :config
    (ivy-prescient-mode +1)
    (setf (alist-get 'counsel-M-x ivy-re-builders-alist)
          #'iby-prescient-re-builder)
    (setf (alist-get t ivy-re-builder-alist) #'ivy--regex-ignore-order))
  (use-package ivy-posframe
    :disabled t
    :delight
    :after ivy
    :hook
    (ivy-mode . ivy-posframe-mode)
    :custom
    (ivy-posframe-display-functions-alist
     '((swiper . nil)
       (swiper-avy . nil)
       (swiper-isearch . nil)
       (counsel-M-x . ivy-posframe-display-at-point)
       (counsel-recentf . ivy-posframe-display-at-frma-center)
       (complete-symbol . ivy-posframe-display-at-point)
       (t . ivy-posframe-display)))
    (ivy-posframe-parameters
     '((left-fringe . 5)
       (right-fringe . 5)))
    (ivy-posframe-height-alist
     '((t . 15)))
    :config
    (ivy-posframe-mode +1))
  (use-package all-the-icons-ivy-rich
    :after all-th-icons ivy
    :init (all-the-icons-ivy-rich-mode 1))
  (use-package ivy-rich
    :delight
    :after ivy all-the-icons-ivy-rich
    :config
    (ivy-rich-mode 1)

    ;;; copy fromhttps://vxlabs.com/2020/11/15/fix-ivy-rich-switch-buffer-directories-display-in-emacs/
    ;; abbreviate turns home into ~ (for example)
    ;; buffers still only get the buffer basename
    (setq ivy-virtual-abbreviation 'abbreviation
          ivy-rich-path-style 'abbrev)
    ;; use buffer-file-name and list-buffers-directory instead of default-directory
    ;; so that special buffers, e.g. *scratch* don't get a directory (we return nil in those cases)
    (defun ivy-rich--switch-buffer-directory (candidate)
      "Return directory of file visited by buffer named CANDIDATE, or nil if no file."
      (let* ((buffer (get-buffer candidate))
             (fn (buffer-file-name buffer)))
        ;; if valid filename, i.e. buffer visiting file:
        (if fn
            ;; return containing directory
            (directory-file-name fn)
          ;; else if mode explicitly offering list-buffers-directory, return that; else nil.
          ;; buffers that don't explicitly visit files, but would like to show a filename,
          ;; e.g. magit or dired, set the list-buffers-directory variable
          (buffer-local-value 'list-buffers-directory buffer))))

    ;; override ivy-rich project root finding to use FFIP or to skip completely
    (defun ivy-rich-switch-buffer-root (candidate)
      ;; 1. changed let* to when-let*; if our directory func above returns nil,
      ;;    we don't want to try and find project root
      (when-let* ((dir (ivy-rich--switch-buffer-directory candidate)))
        (unless (or (and (file-remote-p dir)
                      (not ivy-rich-parse-remote-buffer))
                   ;; Workaround for `browse-url-emacs' buffers , it changes
                   ;; `default-directory' to "http://" (#25)
                   (string-match "https?://" dir))
          (cond
           ;; 2. replace the project-root-finding
           ;; a. add FFIP for projectile-less project-root finding (on my setup much faster) ...
           ((require 'find-file-in-project nil t)
            (let ((default-directory dir))
              (ffip-project-root)))
           ;; b. OR disable project-root-finding altogether
           (t "")
           ((bound-and-true-p projectile-mode)
            (let ((project (or (ivy-rich--local-values
                               candidate 'projectile-project-root)
                              (projectile-project-root dir))))
              (unless (string= project "-")
                project)))
           ((require 'project nil t)
            (when-let ((project (project-current nil dir)))
              (car (project-roots project))))
           ))))
  ))

(use-package multiple-cursors
  :disabled t
  :hook
  (after-init . multiple-cursors)
  :delight
  :bind
  (("C-x r t" . mc/edit-lines)
   ("C-M-SPC" . mc/mark-all-dwim)
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

(use-package iedit
  :config
  (defun iedit-dwim (arg)
    "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
    (interactive "P")
    (if arg
        (iedit-mode)
      (save-excursion
        (widen)
        ;; this function determines the scope of 'iedit-start'.
        (if iedit-mode
            (iedit-done)
          ;; 'current-word' can of course be replaced by other
          ;; functions.
          (narrow-to-defun)
          (iedit-start (current-word) (point-min) (point-max)))))))

(use-package expand-region
  :defer t
  :delight
  :bind
  ("C-=" . er/expand-region))

(use-package avy
  :defer t
  :delight
  :bind
  ("C-c C-j" . avy-resume)
  ("C-'" . avy-goto-char-timer)
  ("M-g g" . avy-goto-line)
  ("M-g w" . avy-goto-word-1)
  :config
  (avy-setup-default))
(use-package avy-migemo
  :after avy migemo
  :delight
  :bind
  ("M-g m" . avy-migemo-mode)
  ("C-M-;" . avy-migemo-goto-char-timer)
  :config
  (avy-migemo-mode 1)
  (setq avy-timeout-seconds 0.5))
(use-package avy-zap
  :after avy
  :delight Zap
  :bind
  ("M-z" . avy-zap-up-to-char-dwim)
  ("M-Z" . avy-zap-up-char-dwim))

(use-package ace-window
  :defer t
  :delight
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
  :delight
  :config
  (beginend-global-mode))

(use-package paredit
  :delight ParEdit)

(use-package smartparens
  :delight SP
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

(use-package lispy
  )

(use-package paren-completer
  :disabled t
  :delight
  :bind
  ("M-)" . paren-completer-add-single-delimiter))

(use-package mic-paren
  :config
  (paren-activate))

(use-package goto-chg
  :bind
  ("<f8>" . goto-last-change)
  ("<M-f8>" . goto-last-change-reverse))

(use-package back-button
  :config
  (back-button-mode 1))

(use-package move-text
  :defer t
  :delight
  :config
  (move-text-default-bindings))

;; (use-package spatial-navigate
;;   :defer t
;;   :delight
;;   :bind
;;   ("<M-up>" . spatial-navigate-backward-vertical-bar)
;;   ("<M-down>" . spatial-navigate-forward-vertical-bar)
;;   ("<M-left>" . spatial-navigate-backward-horizontal-bar)
;;   ("<M-right>" . spatial-navigate-forward-horizontal-bar)
;;   :hook
;;   (after-init-hook . spatial-navigate-mode)
;;   )

(use-package company
  :delight Com
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
  (after-init-hook . global-company-mode)
  :config
  (setq company-require-match nil)
  (setq company-tooltip-align-annotations t)
  (setq company-eclim-auto-save nil)
  (setq company-dabbrev-downcase nil)
  (setq company-selection-wrap-around t)
  (setq company-backends
        '((company-files
           company-keywords
           company-capf)
          (company-abbrev
           company-dabbrev)))

  ;; (add-to-list 'company-backends #'company-tabnine)
  ;; (add-to-list 'company-backends ')

  ;; Enable downcase only when completing the completion.
  (defun jcs--company-complete-selection--advice-around (fn)
    "Advice execute around `company-complete-selection' command."
    (let ((company-dabbrev-downcase t))
      (call-interactively fn)))
  (advice-add 'company-complete-selection
              :around #'jcs--company-complete-selection--advice-around)
  (use-package company-prescient
    :delight
    :after company
    :config
    (company-prescient-mode +1))
  (use-package company-box
    :delight
    :hook
    (company-mode-hook . company-box-mode))
  (use-package company-posframe
    :delight
    :hook
    (company-mode-hook . company-posframe-mode))
  (use-package completions-frame
    :delight
    :hook
    (company-mode . completions-frame-mode))
  ;; (use-package company-tabnine :delight)
  (use-package company-quickhelp
    :when (display-graphic-p)
    :delight
    :bind
    (:map company-active-map
          ("M-h" . company-quickhelp-manual-begin))
    :hook
    (company-mode-hook . company-quickhelp-mode)
    :custom
    (company-quickhelp-delay 0.8))
  (use-package company-quickhelp-terminal
    :delight
    :hook
    (company-mode-hook . company-quickhelp-terminal-mode))
  (use-package company-fuzzy
    :delight
    :hook
    (company-mode-hook . company-fuzzy-mode))
  (use-package company-statistics
    :delight
    :hook
    (company-mode-hook . company-statistics-mode)))

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
          try-complete-lisp-symbol
          yas-hippie-try-expand)))

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

(setq-default abbrev-mode t)
(setq save-abbrevs t)
(setq abbrev-file-name "~/.emacs.d/my-abbreviations.el")
(quietly-read-abbrev-file)

(use-package yasnippet
  :defer t
  :delight Yas
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
  :delight
  :after yasnippet)
(use-package auto-yasnippet
  :after yasnippet
  :delight
                                      ;
  )

(use-package auctex
  :defer t
  :delight AUCTEX
  :hook
  (LaTeX-mode-hook . (turn-on-reftex
                      reftex-mode
                      LaTeX-math-mode
                      outline-minor-mode
                      visual-line-mode))
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
  :after auctex
  :delight cdLaTeX
  :hook
  (LaTeX-mode-hook . turn-on-cdlatex)
  (org-mode-hook . turn-on-org-cdlatex))

(use-package company-auctex
        :delight
        :after company auctex
        :config
        (company-auctex-init))
(use-package company-math
  :delight
  :after company auctex
  :preface
  (defun my/latex-mode-setup ()
    (setq-local company-backends
                (append '((company-math-symbols-latex
                           company-latex-commands
                           company-math-symbols-unicode))
                        company-backends)))
  :hook
  ((org-mode-hook . my/latex-mode-setup)
   (LaTeX-mode-hook . my/latex-mode-setup)))
;; (use-package company-math
;;   :delight
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
;;   (tex-mode-hook . c/latex-mode-setup)))

(use-package eldoc
  :straight nil
  :hook
  ((emacs-lisp-mode-hook lisp-interaction-mode-hook ielm-mode-hook) . (eldoc-mode)))

(use-package elisp-slime-nav
  :hook
  ((emacs-lisp-mode-hook ielm-mode-hook) . elisp-slime-nav-mode))

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
(use-package cl-lib :delight)

(use-package racket-mode
  :defer t
  :delight
  :bind
  (:map racket-mode-map
        ("<f5>" . racket-run))
  :hook
  (racket-mode-hook . racket-xp-mode)
  :config
  (setq tab-always-indent 'complete)
  (setq font-lock-maximum-decoration 3))

(use-package elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

;; (use-package anaconda-mode
;;   :defer t
;;   :hook
;;   (python-mode-hook . (anaconda-mode anaconda-eldoc-mode)))

;; (use-package company-anaconda
;;   :defer t
;;   :preface
;;   (defun my/python-mode-setup ()
;;      (setq-local company-backends
;;                  (append '((company-anaconda))
;;                          company-backends)))
;;   :hook
;;   (python-mode-hook . my/python-mode-setup))

;; (use-package py-yapf
;;   :defer t
;;   :delight
;;   :hook
;;   (python-mode-hook . py-yapf-enable-on-save))

;; (use-package importmagic
;;   :disabled t
;;   :defer t
;;   :hook
;;   (python-mode-hook . importmagic-mode)
;;   :bind
;;   (:map importmagic-mode-map
;;         ("C-c C-f" . importmagic-fix-symbol-at-point)))

(use-package py-isort
  :hook
  (before-save-hook . py-isort-before-save)
  :config
  (setq py-isort-options '("--lines=100")))

(use-package python-pytest)

(use-package ob-ipython
  :defer t)

(use-package julia-mode
  :defer t
  :delight
  :init
  (setq inferior-julia-program-name "/usr/local/bin/julia"))

(use-package julia-repl
  :defer t
  :hook
  (julia-mode-hook . julia-repl-mode))

(use-package markdown-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package quickrun
  :defer t
  :delight)

(use-package dumb-jump
  )

(use-package eldoc-box
  :after eldoc)
(use-package eldoc-overlay
  :after eldoc
  :init (eldoc-overlay-mode 1))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package flycheck-color-mode-line
  :hook
  (flycheck-mode-hook . flycheck-color-mode-line-mode))

(use-package flycheck-pos-tip
  :hook
  (flycheck-mode-hook . flycheck-pos-tip-mode))

(use-package flycheck-status-emoji
  :hook
  (flycheck-mode-hook . flycheck-status-emoji-mode))

(use-package imenu-list
  :bind
  ("C-'" . imenu-list-smart-toggle)
  :config
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-auto-resize t))

(use-package flimenu
  :hook
  (after-init-hook . flimenu-global-mode))

(use-package polymode
  :delight Poly)
(use-package poly-org
  :after polymode
  :delight
  :config
  (add-to-list 'auto-mode-alist
    '("\\.org" . poly-org-mode)))
(use-package poly-markdown
  :after polymode
  :delight
  :config
  (add-to-list 'auto-mode-alist
    '("\\.md" . poly-markdown-mode)))

(use-package org
  :delight Org
  :hook
  (org-mode-hook . (org-indent-mode
                    visual-line-mode
                    variable-pitch-mode))
  :bind
  ("C-c a" . org-agenda)
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
  )

;; Org-Babel tangle
(require 'ob-tangle)
;; Setup Babel languages. Can now do Literate Programming
(org-babel-do-load-languages 'org-babel-load-languages
                             '((python . t)
                               ;; (ein . t)
                               (ipython . t)
                               (shell . t)
                               (emacs-lisp . t)
                               (ledger . t)
                               (ditaa . t)
                               (js . t)
                               (C . t)))

(use-package org-superstar
    :config
    (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(use-package org-web-tools
  :defer t)

(use-package org-cliplink
  :bind
  ("C-x p i" . org-cliplink))

;  (use-package recursive-narrow :defer t :delight)

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
  :delight
  :if (executable-find "aspell")
  :bind
  ("<f12>" . flyspell-mode)
  ("<f10>" . flyspell-buffer)
  :hook
  ((prog-mode . flyspell-prog-mode)
   (LaTeX-mode . flyspell-mode)
   (org-mode . flyspell-mode)
   (text-mode . flyspell-mode)))

(use-package flyspell-correct
  :after flyspell
  :bind
  (:map flyspell-mode-map
        ("C-;" . flyspell-correct-wrapper))
  :config
  (use-package flyspell-correct-ivy
    :after flyspell-correct))

(use-package typo
  :disabled t
  :defer t
  :delight
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
  ;; (setq eshell-history-file-name "~/file_name")
  (setq eshell-history-size 100000)
  (setq eshell-cmpl-cycle-completions nil)
  (setq eshell-cmpl-ignore-case t)
  (setq eshell-glob-include-dot-dot nil)
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
  (use-package eshell-z :delight)
  (use-package eshell-fixed-prompt
    :delight)
  (use-package eshell-prompt-extras
    :delight
    :config
    (setq eshell-highlight-prompt t
          eshell-prompt-function 'epe-theme-lambda))
  (use-package eshell-did-you-mean
    :delight
    :defer t
    :config
    (eshell-did-you-mean-setup))
  (use-package eshell-up
    :delight
    :defer t)
  (use-package esh-autosuggest
    :delight
    :defer t
    :hook (eshell-mode . esh-autosuggest-mode))
  (use-package fish-completion
    :delight
    :hook
    (eshell-mode . global-fish-completion-mode))
  (use-package esh-help
    :delight
    :defer t
    :config
    (setup-esh-help-eldoc)))

(use-package shell-pop
  :defer t
  :bind
  ("C-M-s" . shell-pop)
  :config
  (custom-set-variables
   '(shell-pop-shell-type '("eshell" "*eshell*"
                               (lambda ()
                                 (eshell))))
   '(shell-pop-term-shell "/usr/local/bin/fish")
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
		    :family "IBM Plex Mono"
		    :height 150)
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Noto Sans CJK JP"))
(add-to-list 'face-font-rescale-alist '(".*Noto.*" . 1.2))

(use-package emojify
  :defer t
  :hook
  (after-init . global-emojify-mode))

(set-fontset-font t nil "Symbola")

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
  :delight
  :config
  (global-pretty-mode t))

(use-package apropospriate-theme
  :disabled t
  :config
  (load-theme 'apropospriate-dark t)
  (defvar apropospriate-themes-current-style nil)
  (defun apropospriate-themes-load-style (style)
    "Load apropospriate theme variant STYLE.
    Argument STYLE can be either 'light or 'dark."

    (interactive)
    (cond ((equal style 'light)
           (load-theme 'apropospriate-light t))
          ((equal style 'dark)
           (load-theme 'apropospriate-dark t))

          (t (error (format "Unknown apropospriate theme style: %S" style)))))
  (defun apropospriate-themes-switch-style()
    "Toggle between the light and dark style of apropospriate theme."
    (interactive)
    (cond ((or (null apropospriate-themes-current-style)
               (equal apropospriate-themes-current-style 'dark))
           (apropospriate-themes-load-style 'light)
           (setq apropospriate-themes-current-style 'light))
          ((equal apropospriate-themes-current-style 'light)
           (apropospriate-themes-load-style 'dark)
           (setq apropospriate-themes-current-style 'dark))
          (t (error (format "Invalid apropospriate current style: %s"
                            apropospriate-themes-current-style))))))

(use-package sunburn
  :disabled t
  :straight (sunburn :host github
                     :repo "mvarela/Sunburn-Theme"
                     :branch "master")
  :config
  (load-theme 'sunburn t))

(use-package poet-theme
  :disabled t
  :config
  (load-theme 'poet-dark))

(use-package mood-one-theme
  :disabled t
  :config
  (load-theme 'mood-one t))

(use-package minimal-theme
  :disabled t
  :config
  (load-theme 'minimal-light t))

(use-package berrys-theme
  :disabled t
  :config
  (load-theme 'berrys t))

(use-package bubbleberry-theme
  :disabled t
  :config
  (load-theme 'bubbleberry t))

(use-package tao-theme
  :disabled t
  :config
  (load-theme 'tao-yang t))

(use-package tangotango-theme
  :disabled t
  :config
  (load-theme 'tangotango t))

(use-package tango-plus-theme
  :disabled t
  :config
  (load-theme 'tango-plus t))

(use-package cloud-theme
  :disabled t
  :config
  (load-theme 'cloud t))

(use-package commentary-theme
  :disabled t
  :config
  (load-theme 'commentary t))

(use-package obsidian-theme
  :disabled t
  :config
  (load-theme 'obsidian t))

(use-package eclipse-theme
  :config
  (load-theme 'eclipse t))

;; (use-package modus-themes
;;   :disabled t
;;   :straight nil
;;   :init
;;   (use-package modus-operandi-theme)
;;   (use-package modus-vivendi-theme)
;;   :config
;;   (load-theme modus-vivendi t)
;;   (defvar modus-themes-current-style nil)
;;   (defun modus-themes-load-style (style)
;;     "Load modus theme variant STYLE.
;;     Argument STYLE can be either 'light or 'dark."

;;     (interactive)
;;     (cond ((equal style 'light)
;;            (load-theme 'modus-operandi t))
;;           ((equal style 'dark)
;;            (load-theme 'modus-vivendi t))

;;           (t (error (format "Unknown modus theme style: %S" style)))))
;;   (defun modus-themes-switch-style()
;;     "Toggle between the light and dark style of modus theme."
;;     (interactive)
;;     (cond ((or (null modus-themes-current-style)
;;                (equal modus-themes-current-style 'dark))
;;            (modus-themes-load-style 'light)
;;            (setq modus-themes-current-style 'light))
;;           ((equal modus-themes-current-style 'light)
;;            (modus-themes-load-style 'dark)
;;            (setq modus-themes-current-style 'dark))
;;           (t (error (format "Invalid modus current style: %s"
;;                             modus-themes-current-style))))))

(display-time-mode 1)
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-battery-mode t)
(column-number-mode 1)

(use-package smart-mode-line
  :delight
  :config
  (setq sml/theme 'respectful)
  (setq sml/no-confirm-load-theme t)
  (setq sml/read-only-char "%%")
  (setq sml/modified-char "*")
  (setq sml/extra-filler -10)
  (setq sml/shorten-directory t)
  (setq sml/shorten-modes t)
  ;; (add-to-list
  ;;  'sml/replacer-regexp-list
  ;;  '("^.+/junk/[0-9]+/" ":J:") t)
  (add-to-list 'sml/replacer-regexp-list
               '("^~/Google_drive/" ":GD:") t)
  (add-to-list 'sml/replacer-regexp-list
               '("^~/Dropbox/" ":DB:") t)
  (add-to-list 'sml/replacer-regexp-list
               '("^~/Git_project/" ":Git:") t)
  (setq sml/name-width 20)
  (sml/setup))

(use-package nyan-mode
  :delight
  :custom
  (nyan-cat-face-number 4)
  (nyan-animate-nyancat t)
  :hook
  (after-init . nyan-mode))

(use-package hide-mode-line
  :hook
  ((treemacs-mode imenu-list-minor-mode) . hide-mode-line-mode))

(use-package minions
  :after smart-mode-line
  :config
  (minions-mode 1))

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package dashboard
  :delight dashboard
  :custom
  (dashboard-startup-banner '"~/.emacs.d/image/Larry_Cow.png")
  ;; Value can be
  ;; 'official which displays the official emacs logo
  ;; 'logo which displays an alternative emacs logo
  ;; 1, 2 or 3 which displays one of the text banners
  ;; "path/to/your/image.png" which displays whatever image you would prefer
  (dashboard-center-content t)
  (dashboard-items '((recents .5)
                     (bookmarks .5)))
      ;               (projects .5)
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

(use-package maxframe
  :hook
  (window-setup-hook . (maximize-frame t))
  :config
  (setq mf-max-width 1600)
  )

(use-package edwina
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected))
  (edwina-setup-dwm-keys)
  (edwina-mode 1))

(use-package multicolumn
  :defer t
  :delight
  :init
  (multicolumn-global-mode 1)
  :config
  (setq multicolumn-min-width 72))

(use-package sublimity
  :defer t
  ;; :hook
  ;; (prog-mode-hook . sublimity-mode)
  :config
  (setq sublimity-scroll-weight 5
        sublimity-scroll-drift-length 10)
  (setq sublimity-map-size 20
        sublimity-map-fraction 0.3
        sublimity-map-text-scale -7
        sublimity-map-set-delay 5))

;; Windowmove
(use-package windmove
  :straight nil
  :config
  (windmove-default-keybindings 'super))

(use-package elscreen
  :defer t
  :delight els
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
    :delight
    :defer t
    :config
    (elscreen-separate-buffer-list-mode 1)))
(use-package zoom-window
    :defer t
    :delight
    :bind
    ("C-x z" . zoom-window-zoom)
    :config
    (setq zoom-window-use-elscreen t)
    (zoom-window-setup))

(use-package transpose-frame)

(use-package yequake
  :disabled t
  :custom
  (yequake-frames
   '(("org-capture" 
      (buffer-fns . (yequake-org-capture))
      (width . 0.75)
      (height . 0.5)
      (alpha . 0.95)
      (frame-parameters . ((undecorated . t)
                           (skip-taskbar . t)
                           (sticky . t)))))))

(when (eq system-type 'darwin)
  (use-package ns-auto-titlebar
    :config
    (ns-auto-titlebar-mode)))

(use-package rainbow-mode
  :delight
  :hook ((emacs-lisp-mode c-mode org-mode) . rainbow-mode))
(use-package rainbow-delimiters
  :delight
  :hook
  (prog-mode . rainbow-delimiters-mode))


(use-package highlight-stages
  :defer t
  :delight
  :config
  (highlight-stages-global-mode 1))

(use-package prism
  :defer t
  :delight
  :hook
  (elisp-mode-hook . prism-mode))

(use-package beacon
  :hook
  (after-init . beacon-mode)
  :delight
  :custom
  (beacon-color "#f1fa8c"))

(use-package volatile-highlights
  :defer t
  :delight
  :config
  (volatile-highlights-mode t))

(use-package highlight-indent-guides
  :delight
  :hook
  (prog-mode-hook . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

(use-package line-reminder
  :defer t
  :hook
  (after-init-hook . global-line-reminder-mode)
  :config
  (setq line-reminder-show-option 'indicators)
  (setq line-indicators-fringe-placed 'left-fringe))

(use-package mini-frame
  ;; :disabled t
  :delight
  :hook
  (after-init . mini-frame-mode)
  :config
  (custom-set-variables
   '(mini-frame-show-parameters
     '((top . 0)
       (width . 0.7)
       (left . 0.5)
       ;; (height . 15)
       ))))

(use-package popwin :delight)

(use-package keypression
  :defer t
  :delight
  :config
  (setq keypression-use-child-frame nil
        keypression-fade-out-delya 1.0
        keypression-frame-justify 'keypression-left-justified
        keypression-cast-command-name t
        keypression-cast-command-name-format "%s %s"
        keypression-combine-same-keystrokes t
        keypression-font-face-attribute '(:width normal :height 200 :weight bold))
  (setq keypression-x-offset 100
        keypression-y-offset 100))

(use-package symon
  :defer t
  :delight
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
  :delight
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
  :delight
  :init
  (global-diff-hl-mode))

(use-package pdf-tools
  :defer t
  :magic
  ("%PDF" . pdf-view-mode)
  :hook
  ((pdf-view-mode-hook . (lambda () (display-line-numbers-mode -1)))
   (pdf-view-mode-hook . pdf-tools-enable-minor-modes))
  :config
  (pdf-tools-install)
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (setq pdf-view-resize-factor 1.1))

(use-package pdf-view-restore
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode))

(use-package org-pdftools
  :hook
  (org-load-hook . org-pdftools-setup-link))

(use-package olivetti
  :defer t
  :bind
  ("<f7>" . olivetti-mode)
  :init
  (setq olivetti-body-width 0.618))

(use-package comment-dwim-2
  :defer t
  :delight
  :bind
  ("M-;" . comment-dwim-2))

(use-package comment-or-uncomment-sexp
  :defer t
  :delight
  :bind
  ("C-M-;" . comment-or-uncomment-sexp))

(use-package nocomments-mode
  :defer t
  :delight)

(use-package outline
  :straight nil
  :hook
  (prog-mode-hook . (outline-minor-mode
                     hs-minor-mode)))

(use-package outline-minor-faces
  :after outline
  :config
  (add-hook 'outline-minor-mode-hook
            'outline-minor-faces-add-font-lock-keywords))

(use-package bicycle
  :after outline
  :bind
  (:map outline-minor-mode-map
        ([C-tab] . bicycle-cycle)
        ([S-tab] . bicycle-cycle-global)))

(use-package origami
  :defer t
  :bind
  (:map origami-mode-map
        ("<tab>" . origami-recursively-toggle-node)))

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

(use-package atomic-chrome
  :defer t
  :delight
  :config
  (atomic-chrome-start-server)
  (setq atomic-chrome-buffer-open-style 'split))

(use-package clipmon
  :bind
  ("<M-f2>" . clipmon-autoinsert-toggle))

(use-package igor-mode
  :straight (igor-mode :host github
                       :repo "yamad/igor-mode"
                       :branch "master")
  :defer t)
