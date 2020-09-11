;;; avy-migemo-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "avy-migemo" "avy-migemo.el" (0 0 0 0))
;;; Generated autoloads from avy-migemo.el

(autoload 'avy-migemo-add-names "avy-migemo" "\
Add NAMES to the front of `avy-migemo-function-names'.

\(fn &rest NAMES)" nil nil)

(autoload 'avy-migemo-remove-names "avy-migemo" "\
Remove NAMES from `avy-migemo-function-names'.

\(fn &rest NAMES)" nil nil)

(defvar avy-migemo-mode nil "\
Non-nil if Avy-Migemo mode is enabled.
See the `avy-migemo-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `avy-migemo-mode'.")

(custom-autoload 'avy-migemo-mode "avy-migemo" nil)

(autoload 'avy-migemo-mode "avy-migemo" "\
Override avy's functions.

If called interactively, enable Avy-Migemo mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'avy-migemo-disable-around "avy-migemo" "\
Advice for a function incompatible with `avy-migemo-mode'.
e.g. (advice-add 'counsel-clj :around #'avy-migemo-disable-around)

\(fn ORIG-F &rest ORIG-ARGS)" nil nil)

(autoload 'avy-migemo-regex-cache-clear "avy-migemo" "\
Clear `avy-migemo--regex-cache'." t nil)

(autoload 'avy-migemo-regex-p "avy-migemo" "\
Retrun nil if REGEX is invalid.

\(fn REGEX)" nil nil)

(autoload 'avy-migemo-regex-concat "avy-migemo" "\
Return migemo's regexp which includes PATTERN in last place.
Return PATTERN if migemo's regexp is invalid.
Return quoted PATTERN if PATTERN is invalid.
If NNL-P is non-nil, replace \\s-* on migemo's regexp with empty string.

\(fn PATTERN &optional NNL-P)" nil nil)

(autoload 'avy-migemo-regex-quote-concat "avy-migemo" "\
Return migemo's regexp which includes quoted PATTERN in last place.
Return quoted PATTERN if migemo's regexp is invalid.
If NNL-P is non-nil, replace \\s-* on migemo's regexp with empty string.

\(fn PATTERN &optional NNL-P)" nil nil)

(autoload 'avy-migemo-regex-concat-nnl "avy-migemo" "\
Return migemo's regexp which includes PATTERN with nonnewline.
Replace \\s-* on migemo's regexp with empty string.

\(fn PATTERN)" nil nil)

(autoload 'avy-migemo-regex-quote-concat-nnl "avy-migemo" "\
Return migemo's regexp which includes quoted PATTERN with nonnewline.
Replace \\s-* on migemo's regexp with empty string.

\(fn PATTERN)" nil nil)

(autoload 'avy-migemo-goto-char "avy-migemo" "\
The same as `avy-migemo-goto-char' except for the candidates via migemo.

\(fn CHAR &optional ARG)" t nil)

(autoload 'avy-migemo-goto-char-2 "avy-migemo" "\
The same as `avy-goto-char-2' except for the candidates via migemo.

\(fn CHAR1 CHAR2 &optional ARG BEG END)" t nil)

(autoload 'avy-migemo-goto-char-in-line "avy-migemo" "\
The same as `avy-goto-char-in-line' except for the candidates via migemo.

\(fn CHAR)" t nil)

(autoload 'avy-migemo-goto-char-timer "avy-migemo" "\
The same as `avy-goto-char-timer' except for the candidates via migemo.

\(fn &optional ARG)" t nil)

(autoload 'avy-migemo-goto-subword-1 "avy-migemo" "\
The same as `avy-goto-subword-1' except for the candidates via migemo.

\(fn CHAR &optional ARG)" t nil)

(autoload 'avy-migemo-goto-word-1 "avy-migemo" "\
The same as `avy-goto-word-1' except for the candidates via migemo.

\(fn CHAR &optional ARG BEG END SYMBOL)" t nil)

(autoload 'avy-migemo-isearch "avy-migemo" "\
The same as `avy-isearch' except for the candidates via migemo." t nil)

(autoload 'avy-migemo-org-goto-heading-timer "avy-migemo" "\
The same as `avy-org-goto-heading-timer' except for the candidates via migemo.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "avy-migemo" '("avy-m"))

;;;***

;;;### (autoloads nil "avy-migemo-e.g.counsel" "avy-migemo-e.g.counsel.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from avy-migemo-e.g.counsel.el

(register-definition-prefixes "avy-migemo-e.g.counsel" '("counsel-"))

;;;***

;;;### (autoloads nil "avy-migemo-e.g.ivy" "avy-migemo-e.g.ivy.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from avy-migemo-e.g.ivy.el

(register-definition-prefixes "avy-migemo-e.g.ivy" '("avy-migemo--ivy--regex-hash" "ivy-"))

;;;***

;;;### (autoloads nil "avy-migemo-e.g.swiper" "avy-migemo-e.g.swiper.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from avy-migemo-e.g.swiper.el

(register-definition-prefixes "avy-migemo-e.g.swiper" '("swiper-"))

;;;***

;;;### (autoloads nil nil ("avy-migemo-e.g.zzz-to-char.el") (0 0
;;;;;;  0 0))

;;;***

(provide 'avy-migemo-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; avy-migemo-autoloads.el ends here
