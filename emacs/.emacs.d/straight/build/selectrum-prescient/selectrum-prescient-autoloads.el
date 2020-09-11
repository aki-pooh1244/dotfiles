;;; selectrum-prescient-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "selectrum-prescient" "selectrum-prescient.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from selectrum-prescient.el

(defvar selectrum-prescient-mode nil "\
Non-nil if Selectrum-Prescient mode is enabled.
See the `selectrum-prescient-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `selectrum-prescient-mode'.")

(custom-autoload 'selectrum-prescient-mode "selectrum-prescient" nil)

(autoload 'selectrum-prescient-mode "selectrum-prescient" "\
Minor mode to use prescient.el in Selectrum menus.

If called interactively, enable Selectrum-Prescient mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "selectrum-prescient" '("selectrum-prescient--"))

;;;***

(provide 'selectrum-prescient-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; selectrum-prescient-autoloads.el ends here
