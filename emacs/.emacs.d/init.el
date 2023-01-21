;;; init.el -- Emacs configuration file

;;; Code:


;; Package manager
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


;; package list
(setq my:el-get-packages
      '(; packages
        ;; config
        setup
        ;; Editing
        comment-dwim-2
        page-break-lines
        zop-to-char
        viewer
        undohist
        ;; Search/Replace/Cursor
        goto-chg
        back-button
        deadgrep
        avy
        ;; ace-window
        beginend
        iedit
        ;; Complition
        company-mode
        ;; dired
        dired-hacks
        runner
        ;; key-bind
        key-chord
        smartrep
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
        ;; color-theme
        color-theme-zenburn
        ;; Latex
        magic-latex-buffer
        ;; Python
        anaconda-mode
        ;; emacs-lisp
        flycheck
        ))

(el-get 'sync my:el-get-packages)

;; el-get via github
;; Search/Replace/Cursor
(el-get-bundle minad/vertico)
(el-get-bundle minad/consult)
(el-get-bundle minad/marginalia)
(el-get-bundle oantolin/orderless)

(el-get-bundle leoliu/easy-kill)

(el-get-bundle fourier/loccur)
(el-get-bundle NIcolasPetton/noccur.el
  :name noccur)

(el-get-bundle alezost/mwim.el
  :name mwim)
(el-get-bundle ROCKTAKEY/grugru)

;; dired
(el-get-bundle asok/peep-dired)

;; window
(el-get-bundle cyrus-and/zoom)

;; mode-line
(el-get-bundle tarsius/minions)
(el-get-bundle hinrik/total-lines)

;; misc
(el-get-bundle zk-phi/rpn-calc)
(el-get-bundle elpa:compat)
  

;; zk-phi/setup.el
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


;; Directory

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


;; Editing

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

(global-auto-revert-mode t)

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
  ;; (setq viewer-modeline-color-unwritable "tomato"
  ;;       viewer-modeline-color-view "orange")
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


;; SKK

;; (!-
;;  (setup-lazy
;;    '(skk-mode) "skk-autoloads"
;;    (setup-keybinds nil
;;      "C-x j" 'skk-mode)))


;; Search/Replace/Cursor

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


(setup-lazy
  '(avy-resume avy-goto-char-timer) "avy"
  (avy-setup-default)
  :preapre (setup-keybinds nil
             "M-'" 'avy-goto-char-timer))

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


;; filer

(!-
 (setup-include "dired"
   (setup-keybinds dired-mode-map
     "(" 'dired-hide-details-mode
     ")" 'dired-hide-details-mode)
   (dired-listing-switches "-alh")
   (dired-dwim-target t)
   (dired-recursive-copies 'always)
   (dired-recursive-deletes 'always)
   (delet-by-moving-to-trash t)
   (dired-isearch-filenames 'dwim)
   (dired-ls-F-marks-symlinks t))

 (setup-after "dired"
   (setup-lazy '(wdired-change-to-wdired-mode) "wdired"
     :prepare (setup-keybinds dired-mode-map
                "e" 'wdired-change-to-wdired-mode)
     (wdired-allow-to-change-permission t))
   (setup "dired-aux")
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
   (setup-lazy '(runner) "runner")
   ;; (setup-lazy '(dired-toggle-sudo) "dired-toggle-sudo"
   ;;   :prepare (setup-keybinds dired-mode-map
   ;;              "C-c C-s" 'dired-toggle-sudo))
   ))


;; Completion

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


;; Misc

(ffap-bindings)

(defalias 'yes-or-no-p 'y-or-n-p)
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


;; Save/Backup/Undo

(setup "saveplace"
  (save-place-mode t))

(setq make-backup-files t)
(add-to-list 'backup-directory-alist (cons "." my-backup-directory))

(define-key global-map (kbd "C-z") 'undo)

(setup-include "undohist"
  (undohist-initialize))


;; Keybinds

(define-key global-map (kbd "C-x |") 'split-window-horizontally)
(define-key global-map (kbd "C-x -") 'split-window-vertically)

(!when (eq system-type 'darwin)
  (setq ns-command-modifier 'super)
  (setq ns-option-modifier 'meta))

(setup
  "which-key"
  (which-key-mode))

(setup "key-chord"
  (setq key-chord-two-keys-delay 0.15
        key-chord-one-key-delay 0.15
        key-chord-safety-interval-backward 0.1
        key-chord-safety-interval-forward  0.25)
  (key-chord-mode 1))


;; GUI

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

(global-linum-mode t)

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

;; (setup-include "rainbow-delimiters"
;;   (global-rainbow-delimiters-mode t))


;; Language
;;;LaTeX
(setup-expecting "tex-mode"
  (push '("\\.tex$" . latex-mode) auto-mode-alist)
  (setq latex-run-command "latexmk -pvc")
  (setq latex-run-command "cluttex --engine=lualatex --biber --synctex=1")
  (setq latex-run-command "cluttex --engine=platex --bibtex --synctex=1")
  (setq tex-compile-commands
        '(("latexmk -pvc %f" "%f" "%r.pdf")
          ("cluttex --engine=lualatex --biber --synctex=1 %f" "%f" "%r.pdf")
          ("cluttex --engine=platex --bibtex --synctex=1 %f" "%f" "%r.pdf"))))
(setup-after "tex-mode"
  (setup-hook 'LaTeX-mode-hook
    (visual-line-mode 1)
    (flyspell-mode 1)
    (reftex-mode 1)
    (outline-minor-mode 1)
    (setq-local outline-regexp "\\\\\\(sub\\)*section\\>"
                outline-level (lambda () (- (outline-level) 7))))
  (setup-lazy '(magic-latex-buffer) "magic-latex-buffer"
    :prepare (setup-hook 'latex-mode-hook 'magic-latex-buffer)
    (setq magic-latex-enable-block-highlight t
          magic-latex-enable-pretty-symbols  t
          magic-latex-enable-block-align     t
          magic-latex-enable-inline-image    t
          magic-latex-enable-minibuffer-echo t))
  )

;;;Python
(setup-expecting "python-mode"
  (push '("\\.py$" . anaconda-mode) auto-mode-alist))
(setup-after "python-mode"
  (setup-lazy '(anaconda-mode) "anaconda-mode"
    (setup-hook 'anaconda-mode-hook
      (anaconda-eldoc-mode)
      (setup-lazy '(pipenv-mode) "pipenv"))))

(setup-include "flycheck"
  (global-flycheck-mode))


;; startup
(setq inhibit-startup-screen t
      initial-scratch-message "")


;; Color
;; (setup-include "tangotango-theme"
;;   (load-theme 'tangotango t))
(setup-include "zenburn-theme"
  (load-theme 'zenburn t))



;; Font
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
