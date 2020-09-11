;;; elscreen-separate-buffer-list-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "elscreen-separate-buffer-list" "elscreen-separate-buffer-list.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from elscreen-separate-buffer-list.el

(defvar elscreen-separate-buffer-list-mode nil "\
Non-nil if Elscreen-Separate-Buffer-List mode is enabled.
See the `elscreen-separate-buffer-list-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `elscreen-separate-buffer-list-mode'.")

(custom-autoload 'elscreen-separate-buffer-list-mode "elscreen-separate-buffer-list" nil)

(autoload 'elscreen-separate-buffer-list-mode "elscreen-separate-buffer-list" "\
Toggle elscreen separate buffer list mode.

If called interactively, enable Elscreen-Separate-Buffer-List
mode if ARG is positive, and disable it if ARG is zero or
negative.  If called from Lisp, also enable the mode if ARG is
omitted or nil, and toggle it if ARG is `toggle'; disable the
mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "elscreen-separate-buffer-list" '("esbl-"))

;;;***

(provide 'elscreen-separate-buffer-list-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; elscreen-separate-buffer-list-autoloads.el ends here
