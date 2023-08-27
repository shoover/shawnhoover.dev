;; Usage: emacs --script publish-org-dir.el DIR+
;; Assumes my-org-helpers.el is on the load-path (in site-lisp or via EMACSLOADPATH)

(message "Publishing directories: %s" argv)

;; Use org from the package to avoid warnings about htmlize for formatting source code.
(require 'package)
(package-initialize)
(require 'org)
(message "Using org-version %s" org-version)

(load "my-org-helpers.el") ; org-publish-dir-x, my-org-html-postamble

(setq make-backup-files nil
      auto-save-default nil)

;; Fix coding prompt when writing ox-publish temp files.
(set-language-environment "UTF-8")
;; (add-to-list 'file-coding-system-alist '("" . (undecided . utf-8)) t
;;              (lambda (a b) (equal (car a) (car b))))

;; Something about generating the sitemap causes org-publish to prompt
;; about file locks if another session is editing a file in the project
;; being published. For the purposes of emacs --batch publishing, we
;; can override the default behavior to skip the prompt and just try
;; to take the lock.
(defun ask-user-about-lock (file opponent)
  "Override this standard function to always take the lock."
  (message "[WARN] The publish script is taking the lock on %s previously held by %s"
           file opponent)
  t)

(dolist (dir argv)
  (org-publish-dir-x dir))
