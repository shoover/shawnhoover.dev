;; Usage: emacs --script publish-org-dir.el [--force=t]

(setq force-publish-all (member "--force=t" argv))

;; Use org from the package to avoid warnings about htmlize for formatting source code.
(require 'package)
(package-initialize)
(require 'org)
(message "Publishing with org-mode %s" org-version)

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

(defun pages-html-preamble (options)
  "<div class=\"nav_item fullwidth\"><nav>
<ul><li><a href=\"/\">shawnhoover.dev</a>
<li><a href=\"./\">Pages</a></ul></nav>
</div>")

(defun pages-html-postamble (options)
  "Custom postamble function, because you can't customize the
`org-html-postamble-format' alist in #+options, %d doesn't work,
and dir-locals don't work either.

Set org-html-postamble or #+options: html-postamble."
  (format "<p class=\"date\">Published %s</p>"
          (format-time-string "%d %b %Y")))

(defun org-publish-dir-x (dir target project-name)
  "Publishes all the .org files .css files in DIR to the TARGET
directory using the org HTML publisher."
  (unless (file-exists-p dir)
    (error "Org dir %s does not exist" dir))

  (let* ((dir-exp (expand-file-name dir))
         (org-publish-project-alist
          `((,project-name
             :components ("orgfiles" "css"))
            ("orgfiles"
             :base-directory ,dir-exp
             :publishing-directory ,target
             :publishing-function org-html-publish-to-html
             :auto-sitemap t
             :sitemap-filename "index.org"
             :sitemap-format-entry sitemap-file-title
             :sitemap-ignore-case t
             :sitemap-title ,(format "%s" project-name)
             :makeindex t
             )
            ("css"
             :base-directory ,dir-exp
             :base-extension "css"
             :publishing-directory ,target
             :publishing-function org-publish-attachment)))

         (make-backup-files nil)
         (auto-save-default nil)

         ;; bypass coding system prompt for publish cache if some headlines have unicode
         (default-buffer-file-coding-system 'prefer-utf-8)

         (force force-publish-all))

    ;; Fix coding prompt when writing ox-publish temp files.
    (set-language-environment "UTF-8")

    (message "Publishing org-mode project: %s" dir-exp)
    (org-publish-project project-name force)))


(org-publish-dir-x "pages" "build/pages" "shawnhoover.dev")
