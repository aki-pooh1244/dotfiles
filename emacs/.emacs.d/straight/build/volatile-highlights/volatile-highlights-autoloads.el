;;; volatile-highlights-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "volatile-highlights" "volatile-highlights.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from volatile-highlights.el

(defvar volatile-highlights-mode nil "\
Non-nil if Volatile-Highlights mode is enabled.
See the `volatile-highlights-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `volatile-highlights-mode'.")

(custom-autoload 'volatile-highlights-mode "volatile-highlights" nil)

(autoload 'volatile-highlights-mode "volatile-highlights" "\
Minor mode for visual feedback on some operations.

If called interactively, enable Volatile-Highlights mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "volatile-highlights" '("Vhl/highlight-zero-width-ranges" "vhl/"))

;;;***

(provide 'volatile-highlights-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; volatile-highlights-autoloads.el ends here
