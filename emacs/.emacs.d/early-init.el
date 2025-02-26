;;; early-init.el -- early initialization tweaks -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(setq load-prefer-newer t
      package-enable-at-startup nil
      inhibit-startup-screen t
      initial-buffer-choice nil
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode
      ;; UI
      default-frame-alist '((tool-bar-lines . 0)
		                    (menu-bar-lines . 0)
		                    (vertical-scroll-bars)
		                    (width . 170)
		                    (height . 60)
                            (left . 0)
		                    ;; (fullscreen . maximized)
		                    ))

(setq gc-cons-threshold (* 10 128 1024 1024)
      garbage-collection-message nil)
(setq read-process-output-max (* 8 1024 1024))

(setq byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp) (bytecomp))
      native-comp-async-report-warnings-errors 'silent)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "site-lisp" dir)))

;;; early-init.el ends here.

