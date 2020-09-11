;;; helm-fuzzy-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "helm-fuzzy" "helm-fuzzy.el" (0 0 0 0))
;;; Generated autoloads from helm-fuzzy.el

(defvar helm-fuzzy-mode nil "\
Non-nil if Helm-Fuzzy mode is enabled.
See the `helm-fuzzy-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-fuzzy-mode'.")

(custom-autoload 'helm-fuzzy-mode "helm-fuzzy" nil)

(autoload 'helm-fuzzy-mode "helm-fuzzy" "\
Minor mode 'helm-fuzzy-mode'.

If called interactively, enable Helm-Fuzzy mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-fuzzy" '("helm-fuzzy-")))

;;;***

(provide 'helm-fuzzy-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-fuzzy-autoloads.el ends here
