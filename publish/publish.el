;; Usage: emacs --script publish.el [--force=t] [--dev=t]

(setq force-publish-all (member "--force=t" argv))
(setq notes-dev-mode (member "--dev=t" argv))

(setq script-dir (file-name-directory (if load-in-progress
                                          load-file-name
                                        (buffer-file-name))))

;; Use org from the package to avoid warnings about htmlize for formatting source code.
(require 'package)
(package-initialize)
(require 'org)
(require 'ox-icalendar) ; Workaround ox-rss using icalendar without loading it
(load-file (expand-file-name "ox-rss.el" script-dir))
(load-file (expand-file-name "patch-ox-html.el" script-dir))

(message "Publishing with org-mode %s" org-version)

(defun sitemap-rss-entry (entry style project)
  ":sitemap-format-entry hook to generate entries as subtrees to publish with ox-rss.
Includes link, title, description, and RSS properties."
  (cond ((not (directory-name-p entry))
         (with-temp-buffer
           (org-mode) ; Required for org-set-property to work in 9.6.9
           (let ((title (org-publish-find-title entry project))
                 (description (org-publish-find-property entry :description project 'html))
                 (date (format-time-string "%d %b %Y"
                                           (org-publish-find-date entry project)))
                 (rss-permalink (concat (file-name-sans-extension entry) ".html"))
                 (rss-pubdate (format-time-string
                               (cdr org-time-stamp-formats)
                               (org-publish-find-date entry project))))
             (insert (format "* [[file:%s][%s]]\n%s -- %s\n"
		                     entry
		                     title
                             (or description "")
                             date))

             (org-set-property "RSS_PERMALINK" rss-permalink)
             (org-set-property "PUBDATE" rss-pubdate)
             (org-set-property "ID" rss-permalink))
           (buffer-string)))

	    ((eq style 'tree)
	     ;; Return only last subdir.
	     (file-name-nondirectory (directory-file-name entry)))
	    (t entry)))

(defun sitemap-rss-generate-tree (title project-tree)
  "`:sitemap-function' hook with basic HTML options. Directly
inserts the subtrees from `sitemap-rss-entry'."
  (concat "#+TITLE: " title "\n"
          "#+SETUPFILE: org.txt\n"
          "#+OPTIONS: num:nil html-postamble:sitemap-postamble\n"
          "#+DESCRIPTION: Learnings, thoughts, and references collected in my software work.\n"
          "#+HTML_HEAD: <link rel=\"me\" href=\"https://mastodon.social/@shoover\" />\n"
          "#+HTML_CONTENT_CLASS: sitemap\n"
          "Learnings, thoughts, and references collected in my software work.\n\n"
          (org-list-to-generic
           project-tree
           (list :splice nil :istart "" :icount "" :isep "\n\n"))))

(defun notes-html-preamble (options)
  "<nav>
<ul><li><a href=\"/\">shawnhoover.dev</a></li>
<li><a href=\"/notes\">Notes</a></li>
</ul></nav>")

(defconst notes-footer
  (concat "<hr><p><a href=\"/notes/index.xml\"><img src=\"/assets/icons/feed.svg\" class=\"feed_icon\"/> RSS</a></p>"
          "<p><a rel=\"license\" href=\"http://creativecommons.org/licenses/by/4.0/\"><img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"https://i.creativecommons.org/l/by/4.0/80x15.png\" /></a> This work is licensed under a <a rel=\"license\" href=\"http://creativecommons.org/licenses/by/4.0/\">Creative Commons Attribution 4.0 International License</a>.</p>"))

(defun sitemap-postamble (options)
  notes-footer)

(defun org-publish-dir-x (dir target project-name)
  "Publishes all the .org files .css files in DIR to the TARGET
directory using the org HTML publisher."
  (unless (file-exists-p dir)
    (error "Org dir %s does not exist" dir))

  (let* ((dir-exp (expand-file-name dir))
         (org-publish-project-alist
          `((,project-name
             :components ("orgfiles" "css" "script" "rss"))

            ("orgfiles"
             :base-directory ,dir-exp
             :publishing-directory ,target
             :publishing-function org-html-publish-to-html
             :recursive t
             :exclude "_.*" ; Exclude drafts, e.g. _thoughts.org, _drafts/thoughts.org

             :html-link-home "/notes/"
             :html-link-use-abs-url t
             :html-home/up-format ""
             :html-container "section"
             :html-footnotes-section "<section id=\"footnotes\">
<h2 class=\"footnotes\">%s</h2>
<div id=\"text-footnotes\">
%s
</div>
</section>"

             ,@(when notes-dev-mode
                 '(:html-head-extra "<script src=\"/notes/reload.js\"></script>"))

             :auto-sitemap t
             :sitemap-filename "index.org"
             :sitemap-title ,project-name
             :sitemap-sort-files anti-chronologically
             ;;:sitemap-ignore-case t
             :sitemap-format-entry sitemap-rss-entry
             :sitemap-function sitemap-rss-generate-tree)

            ("css"
             :base-directory ,dir-exp
             :base-extension "css"
             :publishing-directory ,target
             :publishing-function org-publish-attachment)

            ;; Explicitly include hot reloading in dev mode. No other scripts are needed.
            ("script"
             :base-directory ,dir-exp
             :base-extension "js"
             :exclude ".*"
             ,@(when notes-dev-mode
                 '(:include ("reload.js")))
             :publishing-directory ,target
             :publishing-function org-publish-attachment)

            ("rss"
             :base-directory ,dir-exp
             :base-extension "org"
             :publishing-directory ,target
             :publishing-function org-rss-publish-to-rss
             :exclude ".*"
             :include ("index.org")

             :html-link-home "https://shawnhoover.dev/notes"
             :html-link-use-abs-url t

             :rss-title ,(format "shawnhoover.dev - %s" project-name)
             :rss-image-url "https://shawnhoover.dev/assets/icons/apple-touch-icon.png"
             :section-numbers nil
             :table-of-contents nil)))

         (org-html-preamble 'notes-html-preamble)

         (org-html-metadata-timestamp-format "%d %b %Y")
         (org-html-postamble t)
         (org-html-postamble-format
          (list (list "en" (concat "<hr><p class=\"date\">Published: %d</p>"
                                   notes-footer))))

         (make-backup-files nil)
         (auto-save-default nil)

         ;; bypass coding system prompt for publish cache if some headlines have unicode
         (default-buffer-file-coding-system 'prefer-utf-8)

         (force force-publish-all))

    ;; Fix coding prompt when writing ox-publish temp files.
    (set-language-environment "UTF-8")

    (message "Publishing org-mode project: %s" dir-exp)
    (org-publish-project project-name force)))


(org-publish-dir-x "notes" "build/notes" "Notes")

(when nil
  (debug-on-entry 'org-html-link)
  (cancel-debug-on-entry 'org-html-link)

  (cd (concat (file-name-directory (buffer-file-name)) "/.."))

  (org-publish-remove-all-timestamps)

  (let* ((t0 "2023-09-16 Sat 11:59"
             ;;""
             )
         (t1 (org-time-string-to-time t0)))
    (format-time-string "%a, %d %b %Y %T %z" t1))

  (let ((force-publish-all t))
    (org-publish-dir-x "notes" "build/notes" "Notes"))
  )
