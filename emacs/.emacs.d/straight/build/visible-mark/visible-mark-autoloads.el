;;; visible-mark-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "visible-mark" "visible-mark.el" (0 0 0 0))
;;; Generated autoloads from visible-mark.el

(autoload 'visible-mark-mode "visible-mark" "\
A mode to make the mark visible.

If called interactively, enable Visible-Mark mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-visible-mark-mode 'globalized-minor-mode t)

(defvar global-visible-mark-mode nil "\
Non-nil if Global Visible-Mark mode is enabled.
See the `global-visible-mark-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-visible-mark-mode'.")

(custom-autoload 'global-visible-mark-mode "visible-mark" nil)

(autoload 'global-visible-mark-mode "visible-mark" "\
Toggle Visible-Mark mode in all buffers.
With prefix ARG, enable Global Visible-Mark mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Visible-Mark mode is enabled in all buffers where
`visible-mark-mode-maybe' would do it.
See `visible-mark-mode' for more information on Visible-Mark mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "visible-mark" '("global-visible-mark-mode-exclude-alist" "visible-mark-"))

;;;***

(provide 'visible-mark-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; visible-mark-autoloads.el ends here
