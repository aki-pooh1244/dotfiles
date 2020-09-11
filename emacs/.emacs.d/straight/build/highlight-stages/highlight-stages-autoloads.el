;;; highlight-stages-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "highlight-stages" "highlight-stages.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from highlight-stages.el

(autoload 'highlight-stages-mode "highlight-stages" "\
Highlight staged (quasi-quoted) expressions

If called interactively, enable Highlight-Stages mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'highlight-stages-global-mode 'globalized-minor-mode t)

(defvar highlight-stages-global-mode nil "\
Non-nil if Highlight-Stages-Global mode is enabled.
See the `highlight-stages-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `highlight-stages-global-mode'.")

(custom-autoload 'highlight-stages-global-mode "highlight-stages" nil)

(autoload 'highlight-stages-global-mode "highlight-stages" "\
Toggle Highlight-Stages mode in all buffers.
With prefix ARG, enable Highlight-Stages-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Highlight-Stages mode is enabled in all buffers where
`(lambda nil (highlight-stages-mode 1))' would do it.
See `highlight-stages-mode' for more information on Highlight-Stages mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "highlight-stages" '("highlight-stages-"))

;;;***

(provide 'highlight-stages-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; highlight-stages-autoloads.el ends here
