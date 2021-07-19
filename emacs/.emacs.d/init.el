;;; init.el -- Emacs configuration file

;;; Code:

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
(eval-and-compile ; borg
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

(setup "auto-compile"
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest t)
  (setq auto-compile-updatepautoloads t))

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


;; Editing

(setq show-paren-style 'parethesis)
(show-paren-mode +1)
(electric-pair-mode +1)

(setq-default indent-tabs-mode      nil
	      tab-width             4
	      truncate-lines        nil
	      line-move-visual      t
	      fill-column           100
	      require-final-newline t)

(global-auto-revert-mode t)

(!-
 (setup "delsel"
   (delete-selection-mode 1)))

(setup-lazy
  '(comment-dwim-2) "comment-dwim-2"
  :prepare (setup-keybinds nil
	         "M-;" 'comment-dwim-2))

(setup-lazy
  '(iedit-mode) "iedit")

(!-
 (setup "page-ext")
 (setup "page-break-lines"
   (global-page-break-lines-mode)))


;; Search/Replace/Cursor

(!-
 (setup "vertico"
   (vertico-mode))
 (setup "orderless"
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))))

(setup-lazy
  '(avy-resume avy-goto-char-timer) "avy"
  (avy-setup-default)
  (setup-keybinds nil
    "C-'" 'avy-goto-char-timer))

(setup-lazy
  '(ace-window) "ace-window"
  (setq aw-keys '(?j ?k ?l ?i ?o ?h ?y ?u ?p)
        aw-leading-char-face '((t (:height 4.0 :foreground "#f1fa8c"))))
  (setup-keybinds nil
    "s-w" 'ace-window))

(setup "easy-kill"
  (setup-keybinds nil
    "M-w" 'easy-kill
    "C-<SPC>" 'easy-mark))

(setup-lazy
  '(goto-last-change goto-last-change-reverse) "goto-chg"
  (setup-keybinds nil
    "<f8>" 'goto-last-change
    "<M-f8>" 'goto-last-change-reverse))

(setup "back-button"
       (back-button-mode 1))

(setup-lazy
  '(deadgrep) "deadgrep"
  (setup-keybinds nil
    "<f5>" 'deadgrep))


;; filer

(!-
 (setup "dired"
   ))


;; Completion

(!-
 (setup "hippie-exp"
  (setup-keybinds nil
    "C--" 'hippie-expand)
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

(defalias 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map (kbd "SPC") nil)

(prefer-coding-system 'utf-8-unix)

(setup-hook 'minibuffer-setup-hook
  (setq truncate-lines t))

;; Save/Backup/Undo

(setup "saveplace"
  (save-place-mode))


;; Keybinds

(define-key global-map (kbd "C-x -") 'split-window-horizontally)
(define-key global-map (kbd "C-x |") 'split-window-vertically)

(!when (eq system-type 'darwin)
  (setq ns-command-modifier 'meta)
  (setq ns-option-modifier 'super))

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

(setq-default cursor-type 'box)

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


;; startup
(setq inhibit-startup-screen t
      initial-scratch-message "")


;; Color
(setup-include "tangotango-theme"
  (load-theme 'tangotango t))


;; Font
(!when (eq system-type 'darwin)
  (!cond
   ((member "IBM Plex Mono" (font-family-list))
    (set-face-attribute 'default nil :family "IBM Plex Mono" :height 140))))

;;; End here.
