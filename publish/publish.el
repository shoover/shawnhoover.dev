;; A static site generator script for the Dev Notes blog.
;;
;; This is built on the orgmode project system with a mix of static and dynamic
;; customization. `notes-publish' configures and builds a project supporting the
;; following:
;; - HTML generated for .org files in notes/, with navigation/footer templates
;; - HTML sitemap listing for .org files in descending chronological order
;; - RSS feed generated from the sitemap
;; - Hot reloading script included in the pages when invoked in writing mode
;;   (--writing=t is set)
;;
;; Usage: emacs --script publish.el [--force=t] [--writing=t]
;;
;; Requirements:
;; - A recent org-mode package, e.g. 9.6.9 (9.6.6 installed in Emacs 29 doesn't work, IIRC)
;; - ox-rss.el in the same directory. This seems to have been removed from
;;   org-contrib. Also it had key limitations that I've patched.
;; - patch-ox-html.el, until a bug fix is released in org-mode 9.7

;; Process script args.
(setq force-publish-all (member "--force=t" argv))
(setq notes-writing-mode (member "--writing=t" argv))

;; Load orgmode, targeting the package over the built-in.
(require 'package)
(package-initialize)
(require 'org)

;; Load local support functions.
(setq script-dir (file-name-directory (if load-in-progress
                                          load-file-name
                                        (buffer-file-name))))
(load-file (expand-file-name "ox-rss.el" script-dir))
(load-file (expand-file-name "patch-ox-html.el" script-dir))

(message "Publishing with org-mode %s" org-version)


;;
;; Sitemap generation. Generate entries to publish to both an index page and RSS feed.
;;

(defun sitemap-rss-entry (entry style project)
  "`:sitemap-format-entry' hook. Formats entries as subtrees for
publishing to an HTML sitemap and RSS feed. Inserts a link, title,
description, and RSS export properties."
  (cond ((not (directory-name-p entry))
         (with-temp-buffer
           (org-mode) ; Required for org-set-property to work in 9.6.9
           (let* ((title (org-publish-find-title entry project))
                  (description (org-publish-find-property entry :description project 'html))
                  (date (org-publish-find-date entry project))
                  (entry-pubdate (format-time-string "%d %b %Y" date))
                  (rss-permalink (concat (file-name-sans-extension entry) ".html"))
                  (rss-pubdate (format-time-string (cdr org-time-stamp-formats) date)))
             (insert (format "* [[file:%s][%s]]\n%s -- %s\n"
		                     entry
		                     title
                             (or description "")
                             entry-pubdate))

             (org-set-property "RSS_PERMALINK" rss-permalink)
             (org-set-property "PUBDATE" rss-pubdate)
             (org-set-property "ID" rss-permalink))
           (buffer-string)))

	    ((eq style 'tree)
	     ;; Return only last subdir.
	     (file-name-nondirectory (directory-file-name entry)))
	    (t entry)))

(defun sitemap-rss-generate-tree (title project-tree)
  "`:sitemap-function' hook. Creates a sitemap for publishing to an HTML
sitemap and RSS feed. Inserts HTML export properties, a subtitle, and
preformatted subtrees from `sitemap-rss-entry'."
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

(defun sitemap-postamble (options)
  notes-footer)


;;
;; HTML template
;;

(defun notes-html-preamble (info)
  "<nav>
<ul><li><a href=\"/\">shawnhoover.dev</a></li>
<li><a href=\"/notes\">Dev Notes</a></li>
</ul></nav>")

(defun notes-html-postamble (info)
  (format "<hr><p class=\"date\">Published: %s</p>%s"
          (org-export-get-date info (plist-get info :html-metadata-timestamp-format))
          notes-footer))

(defconst notes-footer
  (concat "<hr><p><a href=\"/notes/index.xml\"><img src=\"/assets/icons/feed.svg\" class=\"feed_icon\"/> RSS</a></p>"
          "<p><a rel=\"license\" href=\"http://creativecommons.org/licenses/by/4.0/\"><img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"/assets/images/cca4-80x15.png\" /></a> This work is licensed under a <a rel=\"license\" href=\"http://creativecommons.org/licenses/by/4.0/\">Creative Commons Attribution 4.0 International License</a>.</p>"))


;;
;; Publishing. Wraps a call to `org-publish' using a generated
;; `org-publish-project-alist' to conditionally include hot reloading.
;;

(defun notes-publish (dir target)
  "Publishes my Dev Notes blog from sources in DIR to HTML/RSS in TARGET.
Includes HTML hot reloading if `notes-writing-mode' is non-nil.

Source/target directories are parameterized but could probably be
hardcoded."
  (unless (file-exists-p dir)
    (error "Org dir %s does not exist" dir))

  (let* ((dir-exp (expand-file-name dir))
         (org-publish-project-alist
          `(("Dev Notes"
             :components ("orgfiles" "script" "rss"))

            ;; Generate posts+sitemap HTML
            ("orgfiles"
             :base-directory ,dir-exp
             :publishing-directory ,target
             :publishing-function org-html-publish-to-html
             :recursive t
             :exclude "^_.*" ; Exclude drafts, e.g. _thoughts.org, _drafts/thoughts.org

             :html-link-home "/notes/"
             :html-link-use-abs-url t

             ;; Disable inserting built-in home/up nav links. We control this in the preamble.
             :html-home/up-format ""

             :html-preamble notes-html-preamble
             :html-container "section"
             :html-footnotes-section "<section id=\"footnotes\">
<h2 class=\"footnotes\">%s</h2>
<div id=\"text-footnotes\">
%s
</div>
</section>"
             :html-postamble notes-html-postamble

             ;; Include the hot reload script only when generating the site in
             ;; local writing mode.
             ,@(when notes-writing-mode
                 '(:html-head-extra "<script src=\"/notes/reload.js\"></script>"))

             :auto-sitemap t
             :sitemap-filename "index.org"
             :sitemap-title "Dev Notes"
             :sitemap-sort-files anti-chronologically

             ;; Customize sitemap generation to include all the properties needed to
             ;; generate a feed from the sitemap in the "rss" subproject.
             :sitemap-format-entry sitemap-rss-entry
             :sitemap-function sitemap-rss-generate-tree)

            ;; Build (copy) JS. Only needed for reloading in writing mode.
            ("script"
             :base-directory ,dir-exp
             :base-extension "js"
             :exclude ".*"
             ,@(when notes-writing-mode
                 '(:include ("reload.js")))
             :publishing-directory ,target
             :publishing-function org-publish-attachment)

            ;; Build the feed from the sitemap generated in "orgfiles".
            ("rss"
             :base-directory ,dir-exp
             :base-extension "org"
             :publishing-directory ,target
             :publishing-function org-rss-publish-to-rss
             :exclude ".*"
             :include ("index.org")

             :html-link-home "https://shawnhoover.dev/notes"
             :html-link-use-abs-url t

             :rss-title "shawnhoover.dev - Dev Notes"
             :rss-image-url "https://shawnhoover.dev/assets/icons/apple-touch-icon.png"
             :section-numbers nil
             :table-of-contents nil)))

         (org-html-metadata-timestamp-format "%d %b %Y")

         (make-backup-files nil)
         (auto-save-default nil)

         ;; bypass coding system prompt for publish cache if some headlines have unicode
         (default-buffer-file-coding-system 'prefer-utf-8)

         (force force-publish-all))

    ;; Fix coding prompt when writing ox-publish temp files.
    (set-language-environment "UTF-8")

    (message "Publishing org-mode project: %s" dir-exp)
    (org-publish-project "Dev Notes" force)))


;;
;; Main
;;

(notes-publish "content/notes" "build/notes")

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

  (let ((force-publish-all t)
        (notes-writing-mode t))
    (notes-publish "content/notes" "build/notes"))
  )
