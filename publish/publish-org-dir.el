;; Usage: emacs --script publish-org-dir.el
;; Assumes my-org-helpers.el is on the load-path (in site-lisp or via EMACSLOADPATH)

;; Use org from the package to avoid warnings about htmlize for formatting source code.
(require 'package)
(package-initialize)
(require 'org)
(message "Using org-version %s" org-version)

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

(defun sitemap-file-title (entry style project)
  ":sitemap-format-entry implemention to link the filename and title, and add a date."
  (format "[[file:%s][%s]] %s"
          entry
          (file-name-base entry)
          (org-publish-find-title entry project)))

(defun my-org-html-postamble (options)
  "Custom postamble function, because you can't customize the
`org-html-postamble-format' alist in #+options, %d doesn't work,
and dir-locals don't work either.

Set org-html-postamble or #+options: html-postamble."
  (format "<p class=\"author\">%s</p>
<p class=\"date\">Updated %s</p>
<p class=\"creator\"><a href=\"https://orgmode.org\">orgmode</a></p"
          (car (plist-get options ':author))
          (format-time-string "%d %b %Y")))

(defun org-publish-dir-x (dir target project-name)
  "Publishes all the .org files .css files in DIR to the TARGET
directory using the org HTML publisher."
  (unless (file-exists-p dir)
    (error "Org dir %s does not exist" dir))

  (let* ((dir-exp (expand-file-name dir))
         (org-publish-project-alist `((,project-name
                                       :components ("orgfiles" "css"))
                                      ("orgfiles"
                                       :base-directory ,dir-exp
                                       :publishing-directory ,target
                                       :publishing-function org-html-publish-to-html
                                       :auto-sitemap t
                                       :sitemap-format-entry sitemap-file-title
                                       ;; default alphabetic sort (uses title, not configurable)
                                       :sitemap-ignore-case t
                                       :sitemap-title ,(format "%s index" project-name)
                                       :make-index t
                                       )
                                      ("css"
                                       :base-directory ,dir-exp
                                       :base-extension "css"
                                       :publishing-directory ,target
                                       :publishing-function org-publish-attachment)))
         ;; bypass coding system prompt for publish cache if some headlines have unicode
         (default-buffer-file-coding-system 'prefer-utf-8)

         ;; set to t to bypass the cache and force publishing deleted files
         (force nil))

    ;; Fix coding prompt when writing ox-publish temp files.
    (set-language-environment "UTF-8")
    ;; (add-to-list 'file-coding-system-alist '("" . (undecided . utf-8)) t
    ;;              (lambda (a b) (equal (car a) (car b))))

    (message "Publishing org dir: %s" dir-exp)
    (org-publish-project project-name force)))


(org-publish-dir-x "pages" "build/pages" "shawnhoover.dev")
