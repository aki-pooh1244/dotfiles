;;; awesome-tray-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "awesome-tray" "awesome-tray.el" (0 0 0 0))
;;; Generated autoloads from awesome-tray.el

(defvar awesome-tray-mode nil "\
Non-nil if Awesome-Tray mode is enabled.
See the `awesome-tray-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `awesome-tray-mode'.")

(custom-autoload 'awesome-tray-mode "awesome-tray" nil)

(autoload 'awesome-tray-mode "awesome-tray" "\
Modular tray bar.

If called interactively, enable Awesome-Tray mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "awesome-tray" '("awesome-tray-"))

;;;***

(provide 'awesome-tray-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; awesome-tray-autoloads.el ends here
