;;; super-save-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "super-save" "super-save.el" (0 0 0 0))
;;; Generated autoloads from super-save.el

(defvar super-save-mode nil "\
Non-nil if Super-Save mode is enabled.
See the `super-save-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `super-save-mode'.")

(custom-autoload 'super-save-mode "super-save" nil)

(autoload 'super-save-mode "super-save" "\
A minor mode that saves your Emacs buffers when they lose focus.

If called interactively, enable Super-Save mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "super-save" '("super-save-"))

;;;***

(provide 'super-save-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; super-save-autoloads.el ends here
