;;; init.el -- Emacs configuration file

;;  + Code :


;; + Package manager :
;; Load-Path
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; el-get
(add-to-list 'load-path (locate-user-emacs-file "~/.emacs.d/el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))


;; + | package list
(setq my:el-get-packages
      '(;; packages
        ;; config
        setup
        exec-path-from-shell
        ;; Editing
        comment-dwim-2
        page-break-lines
        zop-to-char
        viewer
        undohist
        ;; pdf-tools
        ;; Search/Replace/Cursor
        goto-chg
        back-button
        deadgrep
        avy
        dumb-jump
        ;; ace-window
        beginend
        iedit
        visual-regexp
        visual-regexp-steroids
        migemo
        ;; Complition
        company-mode
        ;; dired
        dired-hacks
        dired-k
        ;; key-bind
        key-chord
        ;; smartrep
        which-key
        ;; window
        elwm
        zoom-window
        switch-window
        ;; mode-line
        smart-mode-line
        ;; GUI
        diff-hl
        beacon
        indent-guide
        sublimity
        rainbow-delimiters
        ;; color-theme
        color-theme-zenburn
        ;; Latex
        magic-latex-buffer
        ;; python
        anaconda-mode
        ;; racket
        racket-mode
        ;; emacs-lisp
        elisp-slime-nav
        ;; tools
        flycheck
        ;; org-mode
        ;; org-contrib
        org-superstar
        ;; outline-mode
        zoutline
        outline-magic
        ;; eshell
        eshell-autojump
        ))

(el-get 'sync my:el-get-packages)

;; + | el-get via github
;; Search/Replace/Cursor
(el-get-bundle minad/vertico)
(el-get-bundle minad/consult)
(el-get-bundle minad/marginalia)
(el-get-bundle oantolin/orderless)

(el-get-bundle leoliu/easy-kill)

(el-get-bundle fourier/loccur)
(el-get-bundle NIcolasPetton/noccur.el
  :name noccur)

;; eshell
(el-get-bundle 4DA/eshell-toggle)
(el-get-bundle akreisher/eshell-syntax-highlighting)
(el-get-bundle xuchunyang/eshell-git-prompt)
(el-get-bundle peterwvj/eshell-up)
(el-get-bundle tom-tan/esh-help)
(el-get-bundle xuchunyang/eshell-z)
(el-get-bundle mallt/eshell-fixed-prompt-mode)
(el-get-bundle dieggsy/esh-autosuggest)
;; (el-get-git-clone "https://git.jamzattack.xyz/eshell-outline")

(el-get-bundle alezost/mwim.el
  :name mwim)
(el-get-bundle ROCKTAKEY/grugru)
(el-get-bundle AmaiKinono/puni)

(el-get-bundle meow-edit/meow)

;; dired
(el-get-bundle asok/peep-dired)
(el-get-bundle fasheng/dired-toggle)
(el-get-bundle thomp/dired-launch)

;; Language
(el-get-bundle cdominik/cdlatex)
(el-get-bundle Malabarba/latex-extra)
(el-get-bundle hasu/emacs-ob-racket)

;; window
(el-get-bundle cyrus-and/zoom)

;; mode-line
(el-get-bundle tarsius/minions)
(el-get-bundle hinrik/total-lines)

;; misc
(el-get-bundle zk-phi/rpn-calc)
(el-get-bundle elpa:compat)
  

;; + | zk-phi/setup.el
(eval-when-compile
  (require 'setup)
  ;; (require 'cl-lib)
  (setq setup-silent                    t
        setup-delay-interval            0.5
        setup-enable-gc-threshold-hacks t
        ;; setup-use-profiler              t
        ;; setup-use-load-history-tracker  t
        setup-disable-magic-file-name   t))
(setup-initialize)


;; Package manager
;; (eval-and-compile ; borg
;;   (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
;;   (require 'borg)
;;   (borg-initialize))

;; (setup "auto-compile"
;;   (setq auto-compile-display-buffer nil)
;;   (setq auto-compile-mode-line-counter t)
;;   (setq auto-compile-source-recreate-deletes-dest t)
;;   (setq auto-compile-toggle-deletes-nonlib-dest t)
;;   (setq auto-compile-updatepautoloads t))

;; (setup "magit"
;;   setup-after "dash")
;; (with-eval-after-load 'magit
;;   (magit-add-section-hook 'magit-status-section-hook
;; 			  'magit-insert-modules
;; 			  'magit-insert-stashes
;; 			  'append))


;; + Directory :

(eval-and-compile
  (defconst my-data-directory "~/.emacs.d/data/"
    "Directory to save datas."))
(eval-when-compile
  (unless (file-exists-p my-data-directory)
    (make-directory my-data-directory)))

(defconst my-auto-save-list-directory
  (! (concat my-data-directory "auto-save-lists/"))
  "Directory to save auto-save-lists.")

(defconst my-backup-directory
  (! (concat my-data-directory "backups/"))
  "Directory to save backup files.")


;; + Editing :
;; + | misc
(setq show-paren-style 'parethesis)
(show-paren-mode +1)
(electric-pair-mode +1)

(setq-default indent-tabs-mode      nil
	          tab-width             4
	          truncate-lines        t
	          line-move-visual      t
	          fill-column           80
              cursor-type           'box
	          require-final-newline t)

(setq global-auto-revert-mode t
      global-auto-revert-non-file-buffers t
      auto-revert-verbose t)

(!-
 (setup-include "delsel"
   (delete-selection-mode 1)))

(setup-lazy
  '(comment-dwim-2) "comment-dwim-2"
  :prepare (setup-keybinds nil
	         "M-;" 'comment-dwim-2))

(setup-lazy
  '(iedit-mode) "iedit"
  :prepare (setup-keybinds nil
	         "C-;" 'iedit-mode))

(!-
 (setup-include "page-ext")
 (setup-include "page-break-lines"
   (global-page-break-lines-mode)))

(setup-lazy
  '(zop-to-char zop-up-to-char) "zop-to-char"
  :prepare (setup-keybinds nil
             "M-z" 'zop-up-to-char))

(setup-include "viewer"
  (viewer-stay-in-setup)
  (setq viewer-modeline-color-unwritable "tomato"
        viewer-modeline-color-view "orange")
  ;; (viewer-change-modeline-color-setup)
  (viewer-aggressive-setup t))

(setup-lazy
  '(grugru) "grugru"
  :preapre (setup-keybinds nil
             "C-:" 'grugru)
  (grugru-default-setup)
  (grugru-highlight-mode))

(!-
 (setup-include "ispell"
   (setq ispell-program-name "aspell")))

(define-key global-map [165] [92])  ;;¥の代わりにバックスラッシュを入力する

(setup-include "puni"
  (puni-global-mode)
  (setup-hook 'term-mode-hook
    'puni-disable-puni-mode))

;; + | outline
(setup-lazy '(outline-minor-mode) "outline")
(setup-expecting "outline"
  (defvar my-outline-minimum-heading-len 10000)
  (setup-hook 'find-file-hook
    (when (and buffer-file-name (string-match "init\\.el" buffer-file-name))
      (outline-minor-mode 1)
      (setq-local outline-regexp (concat "^\\(\s*" (regexp-quote comment-start)
                                         "[" (regexp-quote comment-start) "]*\\)"
                                         "\s?\\(\s*\\++\\)\s")
                  outline-level  (lambda ()
                                   (setq-local my-outline-minimum-heading-len
                                               (min my-outline-minimum-heading-len
                                                    (- (match-end 0) (match-beginning 0))))
                                   (- (match-end 0) (match-beginning 0)
                                      my-outline-minimum-heading-len)))))
  (setup-lazy '(outline-cycle) "outline-magic"
    :prepare (setup-after "outline"
               (setup-keybinds nil "<C-tab>" 'outline-cycle))))

;; (setup "outline-magic"
;;     (setup-keybinds nil
;;       "<C-tab>" 'outline-cycle))


;; + | SKK

;; (!-
;;  (setup-lazy
;;    '(skk-mode) "skk-autoloads"
;;    (setup-keybinds nil
;;      "C-x j" 'skk-mode)))


;; + | Search/Replace/Cursor

(!-
 (setup "vertico"
   (vertico-mode))
 (setup "orderless"
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))))

(setup-after "vertico"
  (setup "marginalia"
    :prepare (setup-keybinds minibuffer-local-map
               "M-A" 'marginalia-cycle)
    ;; (setq marginalia-align-offset 25)
    (marginalia-mode +1)))

(setup-lazy
  '(consult-line consult-buffer consult-multi-occur) "consult"
  :prepare (setup-keybinds nil
             ;; C-x binds
             "C-x b" 'consult-buffer
             "C-x 4 b" 'consult-buffer-other-window
             "C-x 5 b" 'consult-buffer-other-frame
             ;; C- binds
             "C-s" 'consult-line
             
             ;; M- binds
             "M-y" 'consult-yank-pop))


(setup-include "avy"
  (setup-keybinds nil
    "M-'" 'avy-goto-char-timer
    "<f7>" 'avy-resume)
  (avy-setup-default)
  )

(setup-lazy
  '(switch-window
    switch-window-then-split-below
    switch-window-then-split-right
    switch-window-then-dired
    switch-window-then-find-file
    switch-window-then-display-buffer
    switch-window-then-kill-buffer
    ) "switch-window"
  :prepare (setup-keybinds nil
             "C-x o" 'switch-window
             "s-w" 'switch-window
             "C-x 1" 'switch-window-then-maximize
             "C-x 2" 'switch-window-then-split-below
             "C-x 3" 'switch-window-then-split-right
             "C-x 0" 'switch-window-then-delete
             "C-x 4 d" 'switch-window-then-dired
             "C-x 4 f" 'switch-window-then-find-file
             "C-x 4 C-o" 'switch-window-then-display-buffer
             "C-x 4 0" 'switch-window-then-kill-buffer)
  (setq switch-window-shortcut-style 'qwerty))

;; (setup-include "ace-window"
;;   :preapre (setup-keybinds nil
;;              "s-w" 'ace-window)
;;   (setq aw-keys '(?j ?k ?l ?i ?o ?h ?y ?u ?p)
;;         aw-leading-char-face '((t (:height 4.0 :foreground "#f1fa8c")))))

(setup "easy-kill"
  :preapre (setup-keybinds nil
             "M-w" 'easy-kill
             "C-<SPC>" 'easy-mark))

(setup-lazy
  '(goto-last-change goto-last-change-reverse) "goto-chg"
  :prepare (setup-keybinds nil
             "<f8>" 'goto-last-change
             "<M-f8>" 'goto-last-change-reverse))

(setup-include "back-button"
  (back-button-mode 1))

(setup-lazy
  '(deadgrep) "deadgrep"
  :prepare (setup-keybinds nil
             "<f5>" 'deadgrep))

(setup-lazy '(loccur loccur-current loccur-isearch) "loccur"
  :prepare (setup-keybinds nil
             "C-o"     'loccur-current
             "C-M-o"   'loccur
             "C-O"     'loccur-previous-match
             "M-s C-o" 'loccur-isearch))

(setup-lazy '(noccur-project noccur-dired) "noccur")

(setup-include "mwim"
  :prepare (setup-keybinds nil
             "C-a" 'mwim-beginning
             "C-e" 'mwim-end))

(setup-include "beginend"
  (beginend-global-mode))

;; (setup-lazy '(dumb-jump-go dumb-jump-back) "dumb-jump"
;;   :prepare (setup-keybinds nil
;;              "<f11>" 'dumb-jump-go
;;              "<f12>" 'dumb-jump-back))

(setup-include "visual-regexp"
  (setup-keybinds nil
    "M-%" 'vr/query-replace
    "C-M-r" 'vr/isearch-backward
    "C-M-s" 'vr/isearch-forward))
;; (setup-include "visual-regexp-steroids")

;; (setup-include "migemo"
;;   (setq migemo-command "cmigemo"
;;         migemo-options '("-q" "--emacs" "-i" "\a")
;;         migemo-user-dictionary nil
;;         migemo-regex-dictionary nil
;;         migemo-coding-system 'utf-8-unix)
;;   (setq migemo-dictinary
;;         (cond ((eq system-type 'windows-nt) "dict/utf-8/migemo-dict")
;;               ((eq system-type 'gnu/linux) "/usr/share/cmigemo/utf-8/migemo-dict"))))


;; + Dired :
;; + | dired-main
(!-
 (setup-include "dired"
   (setup-keybinds dired-mode-map
     "(" 'dired-hide-details-mode
     ")" 'dired-hide-details-mode)
   (dired-listing-switches "-alh -L --group-directories-first")
   (dired-dwim-target t)
   (dired-recursive-copies 'always)
   (dired-recursive-deletes 'always)
   (delet-by-moving-to-trash t)
   (dired-isearch-filenames 'dwim)
   (dired-ls-F-marks-symlinks t))

 (setup-lazy '(dired-toggle) "dired-toggle"
     :prepare (setup-keybinds nil
                  "<f6>" 'dired-toggle)
     (setq dired-toggle-window-size 20)
     (setq dired-toggle-window-side 'left)
     (setup-hook 'dired-toggle-mode-hook
       (lambda () (interactive)
         (visual-line-mode 1)
         (setq-local visual-line-fringe-indicators '(nil right-curly-arrow))
         (setq-local word-wrap nil))))
 ;; + | 3rd-party
 (setup-after "dired"
   (setup-lazy '(wdired-change-to-wdired-mode) "wdired"
     :prepare (setup-keybinds dired-mode-map
                "e" 'wdired-change-to-wdired-mode)
     (wdired-allow-to-change-permission t))
   
   (setup "dired-k"
     :prepare (setup-hook 'dired-initial-position-hook
                (dired-k)))
   (setup "dired-aux")
   (setup "dired-launch"
     (dired-launch-enable)
     (setup-keybinds dired-launch-mode-map
       "l" 'dired-launch-command))
   ;; dired-hacks starts
   (setup "dired-hacks-utils")
   (setup "dired-filter"
     (setup-keybinds dired-mode-map
       "/" 'dired-filter-map))
   (setup "dired-narrow"
     (setup-keybinds dired-mode-map
       "f" 'dired-narrow-fuzzy))
   (setup "dired-rainbow"
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
   (setup "dired-collapse")
   (setup "dired-avfs")
   (setup "dired-subtree"
     (setup-keybinds dired-mode-map
       "i" 'dired-subtree-toggle))
   ;; dired-hacks ends here
   (setup-lazy '(peep-dired) "peep-dired"
     :prepare (setup-keybinds dired-mode-map
                "g" 'peep-dired))
   ;; (setup-lazy '(quick-preview-at-point) "quick-preview"
   ;;   :prepare (setup-keybinds nil
   ;;              "C-c q" 'quick-preview-at-point)
   ;;   :prepare (setup-keybinds dired-mode-map
   ;;              "Q" 'quick-preview-at-point))
   ;; (setup-lazy '(runner) "runner")
   ;; (setup-lazy '(dired-toggle-sudo) "dired-toggle-sudo"
   ;;   :prepare (setup-keybinds dired-mode-map
   ;;              "C-c C-s" 'dired-toggle-sudo))
   ))


;; + Completion :

(!-
 (setup "hippie-exp"
  :prepare (setup-keybinds nil
             "M-/" 'hippie-expand)
  (setq hippie-expand-try-function-list
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
          yas-hippie-try-expand))))

(!-
 (setup "company"
   (setq company-idle-delay 0
         company-require-match 'never
         company-dabbrev-downcase nil
         company-minimum-prefix-length 2
         company-selection-wrap-around t
         company-tooltip-align-annotations t)
   (global-company-mode)))


;; + Misc :

(ffap-bindings)

;; (defalias 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t)
(define-key query-replace-map (kbd "SPC") nil)

(prefer-coding-system 'utf-8-unix)

;; (setup-hook 'minibuffer-setup-hook
;;   (setq truncate-lines t))

(setup-include "sublimity-scroll"
  (sublimity-mode 1))

;; (setup-include "nyan-mode"
;;   (setq nyan-bar-length 10)
;;   (nyan-mode 1))

(setup-lazy '(rpn-calc) "rpn-calc")

(setup-hook 'after-save-hook
  (lambda ()
    (let ((orig-fg (face-background 'mode-line)))
      (set-face-background 'mode-line "dark green")
      (run-with-idle-timer 0.1 nil
                           (lambda (fg) (set-face-background
                                         'mode-line fg))
                           orig-fg))))


;; + Save/Backup/Undo :

(setup "saveplace"
  (save-place-mode t))

(setq make-backup-files t)
(add-to-list 'backup-directory-alist (cons "." my-backup-directory))

(define-key global-map (kbd "C-z") 'undo)

(setup-include "undohist"
  (undohist-initialize))


;; + Keybinds :

(define-key global-map (kbd "C-x |") 'split-window-horizontally)
(define-key global-map (kbd "C-x -") 'split-window-vertically)

(!when (eq system-type 'darwin)
  (setq ns-command-modifier 'super)
  (setq ns-option-modifier 'meta))

(setup
  "which-key"
  (which-key-mode))
;; + | key-chord
(setup "key-chord"
  (setq key-chord-two-keys-delay 0.15
        key-chord-one-key-delay 0.15
        key-chord-safety-interval-backward 0.1
        key-chord-safety-interval-forward  0.25)
  (key-chord-mode 1))

(setup "meow"
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
  (meow-global-mode 1))


;; + GUI :

(!when (eq window-system 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;; https://qiita.com/fujimotok/items/28b03a05224cedd0cc5a
(!when (window-system)
       (setq display-time-string-forms
             '((format "%s/%s/%s" year month day)
               (format "(%s:%s)" 24-hours minutes)))
       (display-time)
       (setq frame-title-format
             '("" global-mode-string
               (:eval (if (buffer-file-name) " %f" " %b")))))
;; (setq frame-title-format
;;       '("emacs%@"
;; 	    (:eval (system-name)) ": "
;; 	    (:eval (if (buffer-file-name)
;; 		           (abbreviate-file-name (buffer-file-name))
;; 		         "%b")) " [%*]"))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))

(setup "diff-hl"
  (global-diff-hl-mode))

;; + | modeline

;; (setup "moody"
;;   (setq x-underline-at-descent-line t)
;;   (moody-replace-mode-line-buffer-identification)
;;   (moody-replace-vc-mode)
;;   (moody-replace-eldoc-minibuffer-message-function))

(setup-include "minions"
  (minions-mode t))

(setup-include "total-lines"
  (global-total-lines-mode t)
  (setq-default mode-line-front-space
                (append mode-line-front-space
                        '((:eval (format " (%d)" (- total-lines 1))))))
  )

(setup-include "smart-mode-line"
  (setq sml/theme 'respectful)
  (setq sml/no-confirm-load-theme t)
  (setq sml/modified-char "*")
  ;; (setq sml/mode-width 20)
  ;; (setq sml/extra-filler -10)
  (setq sml/shorten-directory t)
  (add-to-list 'sml/replacer-regexp-list
               '("^~/git_projects/aki-pooh1244/dotfiles/emacs/.emacs.d" ":ED:")
               t)
  (add-to-list 'sml/replacer-regexp-list
               '("^~/git_projects/aki-pooh1244/" ":MyGit:") t)
  (add-to-list 'sml/replacer-regexp-list
               '("^~/git_projects/" ":Git:") t)
  (sml/setup))

(column-number-mode t)

(global-display-line-numbers-mode t)
;; (global-linum-mode t)

;; + | others
(size-indication-mode t)

(setup-lazy '(indent-guide-mode) "indent-guide"
  (set-face-background 'indent-guide-face "lightpink"))

(setup-include "beacon"
  (beacon-mode 1)
  (setq beacon-color "lightpink"))

(setup-hook 'prog-mode-hook
  (setup-lazy '(display-fill-column-indicator-mode) "display-fill-column-indicator"
    (setq-default display-fill-column-indicator 80)))

;; (setup-lazy '(visual-fill-column-mode) "visual-fill-column"
;;     :prepare (setup-keybinds nil
;;                "C-c t" 'visual-line-mode)
;;     :prepare (setup-hook 'visual-line-mode-hook 'visual-fill-column-mode))

;; (setup-include "hiwin"
;;   (hiwin-activate))

;; + | flame
(setup-include "elwm")

(setup-lazy '(zoom-window-zoom) "zoom-window"
  :prepare (setup-keybinds nil
             "C-x C-z" 'zoom-window-zoom)
  (custom-set-variables
   '(zoom-window-mode-line-color "DarkGreen")))

(setup-include "zoom"
  (zoom-mode t)
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618))
   '(zoom-ignored-major-modes '(dired-mode markdown-mode))
   '(zoom-ignored-buffer-name-regexps '("^*calc"))
   '(zoom-ignore-predicates '((lambda () (> (count-lines (point-min) (point-max)) 20))))))

(setup-include "rainbow-delimiters"
  (setq global-rainbow-delimiters-mode t))


;; + Language :
;; + | LaTeX
(setup-expecting "tex-mode"
  (push '("\\.tex$" . latex-mode) auto-mode-alist))

(setup-after "tex-mode"
  (push "Verbatim" tex-verbatim-environments)
  (push "BVerbatim" tex-verbatim-environments)
  (push "lstlisting" tex-verbatim-environments)
  (setup-hook 'latex-mode-hook
    (visual-line-mode 1)
    (flyspell-mode 1)
    (reftex-mode 1)
    (bibtex-mode 1)
    (outline-minor-mode 1)
    (setq-local outline-regexp "\\\\\\(sub\\)*section\\>"
                outline-level  (lambda () (- (outline-level) 7))))
  (setup-lazy '(magic-latex-buffer) "magic-latex-buffer"
    :prepare (setup-hook 'latex-mode-hook 'magic-latex-buffer)
    (setq magic-latex-enable-inline-image nil))
  (setup-lazy '(cdlatex-mode) "cdlatex"
    :prepare (setup-hook 'latex-mode-hook 'turn-on-cdlatex))
  (setup-lazy '(reftex-mode) "reftex"
    :prepare (setup-hook 'latex-mode-hook 'turn-on-reftex)))
;; (setup-expecting "latex-mode"
;;   (push '("\\.tex$" . latex-mode) auto-mode-alist)
;;   (setq latex-run-command "latexmk -pvc")
;;   (setq latex-run-command "cluttex --engine=lualatex --biber --synctex=1")
;;   (setq latex-run-command "cluttex --engine=platex --bibtex --synctex=1")
;;   (setq tex-compile-commands
;;         '(("latexmk -pvc %f" "%f" "%r.pdf")
;;           ("cluttex --engine=lualatex --biber --synctex=1 %f" "%f" "%r.pdf")
;;           ("cluttex --engine=platex --bibtex --synctex=1 %f" "%f" "%r.pdf")))
  
;;   )
;; (setup-hook 'latex-mode-hook
;;     (setq visual-line-mode 1)
;;     (setq flyspell-mode 1)
;;     (setq reftex-mode 1)
;;     (outline-minor-mode)
;;     (setq-local outline-regexp "\\\\\\(sub\\)*section\\>"
;;                 outline-level (lambda () (- (outline-level) 7)))
;;     )
;; (setup-lazy '(magic-latex-buffer) "magic-latex-buffer"
;;     :prepare (setup-hook 'latex-mode-hook 'magic-latex-buffer)
;;     (setq magic-latex-enable-block-highlight t
;;           magic-latex-enable-pretty-symbols  t
;;           magic-latex-enable-block-align     t
;;           magic-latex-enable-inline-image    t
;;           magic-latex-enable-minibuffer-echo t))
;; (setup-lazy '(latex-extra-mode) "latex-extra"
;;     :prepare (setup-hook 'latex-mode-hook 'latex-extra-mode))
;; (setup-lazy '(cdlatex-mode) "cdlatex"
;;     :prepare (setup-hook 'latex-mode-hook 'turn-on-cdlatex))
;; (setup-after "tex-mode"
  ;; (setup-hook 'latex-mode-hook
  ;;   (setq visual-line-mode 1)
  ;;   (setq flyspell-mode 1)
  ;;   (setq reftex-mode 1)
  ;;   (setq outline-minor-mode 1)
  ;;   (setq-local outline-regexp "\\\\\\(sub\\)*section\\>"
  ;;               outline-level (lambda () (- (outline-level) 7))))
  ;; (setup-lazy '(magic-latex-buffer) "magic-latex-buffer"
  ;;   :prepare (setup-hook 'latex-mode-hook 'magic-latex-buffer)
  ;;   (setq magic-latex-enable-block-highlight t
  ;;         magic-latex-enable-pretty-symbols  t
  ;;         magic-latex-enable-block-align     t
  ;;         magic-latex-enable-inline-image    t
  ;;         magic-latex-enable-minibuffer-echo t))
  ;; (setup-lazy '(latex-extra-mode) "latex-extra"
  ;;   :prepare (setup-hook 'latex-mode-hook 'latex-extra-mode))
  ;; (setup-lazy '(cdlatex-mode) "cdlatex"
  ;;   :prepare (setup-hook 'latex-mode-hook 'turn-on-cdlatex))
;; )

;; + | Python
(setup-expecting "python-mode"
  (push '("\\.py$" . anaconda-mode) auto-mode-alist))
(setup-after "python-mode"
  (setup-lazy '(anaconda-mode) "anaconda-mode"
    (setup-hook 'anaconda-mode-hook
      (anaconda-eldoc-mode)
      (setup-lazy '(pipenv-mode) "pipenv"))))

;; + | emacs-lisp
(setup-expecting "emacs-lisp-mode"
  (setup-hook '(emacs-lisp-mode-hook ielm-mode-hook)
    (elisp-slime-nav-mode)))

;; + | Racket-lang
(setup-expecting "racket-mode"
  (push '("\\.scrbl$" . scribble-mode) auto-mode-alist)
  (push '("\\.rkt$" . racket-mode) auto-mode-alist)
  (setup-lazy '(scribble-mode) "scribble")
  (setup-lazy '(racket-mode) "racket-mode"))


;; + Tools :
(setup-include "flycheck"
  (global-flycheck-mode))


;; + org-mode :
;; + | org
(setup "org"
  (require 'org-loaddefs)
  (setup-keybinds nil
    "C-c c" 'org-capture
    "C-c a" 'org-agenda
    "C-c l" 'org-store-link
    "C-c b" 'org-iswitchb)
  
  (setq org-directory "~/Dropbox/org/")
  
  (setq org-startup-folded t
        org-startup-indented t
        org-startup-with-inline-images t
        org-src-fontify-natively t
        org-src-tab-acts-natively t)
  
  ;;  + Capture
  (setq org-capture-templates
        '(("t" "TODO" entry (file "~/Dropbox/org/todo.org")
           "* TODO %?\n %u\n %i\n")
          ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
           "* %?\nEntered on %U\n %i\n")
          ("s" "snippets" entry (file "~/Dropbox/org/snippets.org")
           "* %?\n %U\n %a")))
  
  ;;  + TODO
  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)" "|" "SOMEDAY(p)")))
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("STARTED" :foreground "cyan" :weight bold)
                ("DONE" :foreground "green" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                ("SOMEDAY" :foreground "blue" :weight bold))))

  (setq org-enforce-todo-dependencies t
        org-log-done-with-time t
        org-log-done t
        org-clock-out-when-done t
        org-use-fast-todo-selection t
        org-clock-out-remove-zero-time-clocks t)
  
  (setq org-clock-in-switch-to-state 'org-clock-in-to-started)

  (setq org-archive-location "archive.org::")

  ;;  + Agenda
  (setq org-agenda-files (list org-directory))
  (setq org-agenda-span 'day
        org-agenda-time-leading-zero t)

  ;;  + Third-party
  (setup "org-superstar"
    (setq org-superstar-mode t))

  ;;  + def function
  (defun show-org-buffer (file)
    "Show an org-file on the current buffer"
    (interactive)
    (if (get-buffer file)
        (let ((buffer (get-buffer file)))
          (switch-to-buffer buffer)
          (message "%s" file))
      (find-file (concat "~/Dropbox/org/" file))))
  (setup-keybinds nil
    "C-M-c" '(lambda () (interactive)
               (show-org-buffer "journal.org")))

  (defun org-clock-in-to-started (state)
    (if (or (string= state "TODO")
            (string= state "WAITING"))
        "STARTED"))

  ;;  + org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (racket . t)
     (python . t)
     (dot . t)
     (latex . t)
     ))
  (setup "ox-latex")
  )

;; + Eshell :
(setup-lazy '(eshell-toggle) "eshell-toggle"
  :prepare (setup-keybinds nil
             "M-t" 'eshell-toggle)
  (setq eshell-toggle-size-fraction 3
        eshell-toggle-default-directory "~/"
        eshell-toggle-run-command nil))

(setup-after "eshell"
  (setup "eshell-syntax-highlighting"
    (eshell-syntax-highlighting-global-mode +1))
  (setup "eshell-git-prompt"
    (eshell-git-prompt-use-theme 'default))
  (setup "eshell-up"
    (setq eshell-up-print-parent-dir t))
  (setup "eshell-autojump")
  (setup "esh-help"
    (setup-esh-help-eldoc))
  (setup "eshell-z")
  (setup "eshell-fixed-prompt")
  (setup "esh-autosuggest"))


;; + startup :
(setq inhibit-startup-screen t
      initial-scratch-message "")


;; + Color :
;; (setup-include "tangotango-theme"
;;   (load-theme 'tangotango t))
(setup-include "zenburn-theme"
  (load-theme 'zenburn t))



;; + Font :
(!when (eq system-type 'darwin)
  (!cond
   ((member "IBM Plex Mono" (font-family-list))
    (set-face-attribute 'default nil :family "IBM Plex Mono" :height 140))))
(!when (eq system-type 'gnu/linux)
  (!cond
   ((member "IBM Plex Mono" (font-family-list))
    (set-face-attribute 'default nil :family "IBM Plex Mono" :height 120))
   ((member "Noto Sans CJK JP" (font-family-list))
    (set-face-attribute '(han kana) nil :family "Noto Sans CJK JP Regular" :height 120))))

(!when (eq system-type 'windows-nt)
  (!cond
   ((member "PlemolJP" (font-family-list))
    (set-face-attribute 'default nil :family "PlemolJP" :height 120))))

;;; End here.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(compat zenburn-theme))
 '(zoom-ignore-predicates
   '((lambda nil
       (>
        (count-lines
         (point-min)
         (point-max))
        20))))
 '(zoom-ignored-buffer-name-regexps '("^*calc"))
 '(zoom-ignored-major-modes '(dired-mode markdown-mode))
 '(zoom-size '(0.618 . 0.618)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
