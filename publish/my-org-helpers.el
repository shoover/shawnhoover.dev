
(defun find-org-files-x (&optional regexp)
  "Returns a list of files in `org-directories' (searched
recursively) whose names match REGEXP. The search pattern
defaults to .org. You can override, for example, to also search
archives."
  (let ((regexp (or regexp "\\.org$")))
    (apply 'append
           (mapcar (lambda (dir)
                     (directory-files dir t regexp))
                   org-directories))))

(defun org-promote-subtree-x (&optional n)
  "Cut the current subtree and paste it one heading level up.
With prefix arg N, cut this many sequential subtrees.

There is a built in `org-promote-subtree', but this cut/paste
style works more intuitively with respect to the parent level and
siblings."
  (interactive)
  (org-cut-subtree n)
  (outline-up-heading 1)
  (org-paste-subtree)

  ;; Workaround so repeated calls don't append to the previous promoted text
  ;; and paste duplicate info. org-cut-subtree leaves last-command set to
  ;; kill-region but org-paste-subtree doesn't set it.
  (setq this-command 'org-promote-subtree-x))

(defun org-convert-items-to-paragraphs-x ()
  "Convert items to normal paragraphs. If there is no active
region, only the current line is considered.

The overall structure and helper functions are cribbed from
`org-toggle-item', which can convert items to plain lines but
doesn't unindent multiline item text."
  (interactive)
  (let ((shift-text
         (function
          ;; Shift text in current section to IND, from point to END.
          ;; The function leaves point to END line.
          (lambda (ind end)
            (let ((min-i 1000) (end (copy-marker end)))
              ;; First determine the minimum indentation (MIN-I) of
              ;; the text.
              (save-excursion
                (catch 'exit
                  (while (< (point) end)
                    (let ((i (org-get-indentation)))
                      (cond
                       ;; Skip blank lines and inline tasks.
                       ((looking-at "^[ \t]*$"))
                       ((looking-at org-outline-regexp-bol))
                       ;; We can't find less than 0 indentation.
                       ((zerop i) (throw 'exit (setq min-i 0)))
                       ((< i min-i) (setq min-i i))))
                    (forward-line))))
              ;; Then indent each line so that a line indented to
              ;; MIN-I becomes indented to IND.  Ignore blank lines
              ;; and inline tasks in the process.
              (let ((delta (- ind min-i)))
                (while (< (point) end)
                  (unless (or (looking-at "^[ \t]*$")
                              (looking-at org-outline-regexp-bol))
                    (org-indent-line-to (+ (org-get-indentation) delta)))
                  (forward-line)))))))
        (skip-blanks
         (function
          ;; Return beginning of first non-blank line, starting from
          ;; line at POS.
          (lambda (pos)
            (save-excursion
              (goto-char pos)
              (skip-chars-forward " \r\t\n")
              (point-at-bol)))))
        beg end)

    ;; Determine boundaries of changes.
    (if (org-region-active-p)
        (setq beg (funcall skip-blanks (region-beginning))
              end (copy-marker (region-end)))
      (setq beg (funcall skip-blanks (point-at-bol))
            end (copy-marker (point-at-eol))))

    ;; Depending on the starting line, choose an action on the text
    ;; between BEG and END.
    (org-with-limited-levels
     (save-excursion
       (goto-char beg)
       (let ((ref-ind (org-get-indentation))
             (first-item t))
         (while (< (point) end)
           (cond
            ;; Start at an item: de-itemize and insert blank lines between them.
            ((org-at-item-p)
             (progn
               ;; Delete the item prefix.
               (save-excursion
                 (skip-chars-forward " \t")
                 (delete-region (point) (match-end 0)))

               ;; Insert the blank line after deleting the item prefix;
               ;; otherwise the prefix search gets messed up by a character or
               ;; two.
               (if first-item
                   (setq first-item nil)
                 (newline))))

            ;; Unindent lines of enclosed paragraph.
            (t
             (save-excursion
               (funcall shift-text
                        ref-ind
                        (min end (save-excursion
                                   (move-end-of-line nil)
                                   (point)))))))

           (forward-line)))))))

(require 'browse-url)
(defun org-export-html-subtree-x (arg)
  "Exports the current subtree to HTML and browses to the file.

With a prefix arg, prompt for a file name. If property
\"EXPORT_FILE_NAME\" is set in the subtree, that file name is
used. Otherwise a temp file is used.

Defaults to disabling the table of contents and numbers. Override
by setting EXPORT_OPTIONS in the subtree."
  (interactive "P")
  (let ((file (or
               ;; Get the output file name from the user if a prefix arg was given
               (and arg
                    (ido-read-file-name "HTML file:"))

               ;; Use the subtree export file name if the property is set
               (org-entry-get
                (save-excursion
                  (ignore-errors (org-back-to-heading) (point)))
                "EXPORT_FILE_NAME" t)

               ;; Otherwise use a temp file, logic cribbed from browse-url-of-buffer
               (convert-standard-filename
                (make-temp-file
                 (expand-file-name "burl" browse-url-temp-dir)
                 nil ".html"))))
        (async nil)
        (subtreep t)
        (visible-only t)

        ;; I want to override in-buffer options (but not subtree
        ;; options), but it doesn't work and I can't find a way to
        ;; fake properties :(
        (org-export-with-toc nil)
        (org-export-with-section-numbers nil))
    (browse-url-of-file
     (org-export-to-file 'html file async subtreep visible-only))))

(defun org-export-pdf-subtree-x (arg)
  "Exports the current subtree to HTML and browses to the file.

With a prefix arg, prompt for a file name. If property
\"EXPORT_FILE_NAME\" is set in the subtree, that file name is
used. Otherwise a temp file is used."
  (interactive "P")
  (let ((file (or
               ;; Get the output file name from the user if a prefix arg was given
               (and arg
                    (ido-read-file-name "PDF file:"))

               ;; Use the subtree export file name if the property is set
               (org-entry-get
                (save-excursion
                  (ignore-errors (org-back-to-heading) (point)))
                "EXPORT_FILE_NAME" t)

               ;; Otherwise use a temp file, logic cribbed from browse-url-of-buffer
               (convert-standard-filename
                (make-temp-file
                 (expand-file-name "burl" browse-url-temp-dir)
                 nil ".pdf"))))
        (async nil)
        (subtreep t)
        (visible-only t)
        (org-export-with-toc nil))
    (browse-url-of-file
     (org-export-to-file 'latex file async subtreep visible-only))))

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

(defun sitemap-file-title (entry style project)
  ":sitemap-format-entry implemention to link the filename and title, and add a date."
  (format "[[file:%s][%s]] %s"
          entry
          (file-name-base entry)
          (org-publish-find-title entry project)))

(defun org-publish-dir-x (dir &optional target project-name)
  "Publishes all the .org files .css files in DIR to the TARGET
directory using the org HTML publisher."
  (interactive
   (let* ((dir (ido-read-directory-name "Source dir: "))
          (target (ido-read-directory-name "HTML dir: "
                                           (concat dir "/publish")))
          ;; strip trailing slash and get the last path part
          (project-name (file-name-nondirectory (directory-file-name dir)))
          (project-name (read-string (format "Project name [%s]: " project-name)
                                     nil nil project-name)))
     (list dir target project-name)))

  (unless (file-exists-p dir)
    (error "Org dir %s does not exist" dir))

  (let* ((dir-exp (expand-file-name dir))
         (target (or target (concat dir-exp "/publish")))
         (project-name (or project-name (file-name-nondirectory dir)))
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

(defun copy-org-link-at-point ()
  (interactive)
  (when (org-in-regexp org-bracket-link-regexp 1)
    (let ((link (org-link-unescape (org-match-string-no-properties 1))))
      (kill-new link)
      (message "Copied link '%s' to the clipboard." link))))

(defun read-org-agenda-file ()
  "Completing-read for an org agenda file."
  (let ((org-completion-use-ido t))
    (org-icompleting-read "Org buffer: "
                          (mapcar 'list (mapcar 'buffer-name (org-buffer-list 'agenda)))
                          nil t)))

;; org-capture frames, adapted from Lau's remember frames:
;; http://github.com/LauJensen/Configs/blob/master/emacs
(defun capture-frame-p ()
  (equal "*Capture*" (frame-parameter nil 'name)))

;; Automatic closing of capture frames.
(add-hook 'org-capture-after-finalize-hook
          'my-org-capture-after-finalize-hook)
(add-hook 'org-after-refile-insert-hook
          'my-org-after-refile-insert-hook)

(defun my-org-capture-after-finalize-hook ()
  (when (and (frame-live-p (selected-frame))
             (capture-frame-p)

             ;; Don't delete the frame yet if we finalized as part of
             ;; refiling. Refile is not done yet and has to be handled
             ;; separately.
             (not (and (boundp 'org-capture-is-refiling)
                       org-capture-is-refiling)))
    (delete-frame)))

(defun my-org-after-refile-insert-hook ()
  (when (and (frame-live-p (selected-frame))
             (capture-frame-p))
    (delete-frame)))

(defun make-capture-frame ()
  "Makes a frame with dialog-like properties and the name
\"*Capture*\" for tracking in after-finalize."
  (require 'frame-center)
  (let ((f (make-frame `((name . "*Capture*")
                         (width . 95) (height . 25)))))
    (frame-bottom-right f)
    (select-frame f)
    (raise-frame f)
    f))

(defmacro with-capture-frame (&rest body)
  "Opens a new capture frame and invokes BODY. This also hacks
org-mode variables to make sure org capture template selection
doesn't divide the frame into a new window; since we're already
making a dedicated frame, we want one full window in the frame.

Returns with the capture frame ready for entry, i.e. not entered
and finalized."
  `(progn
     (let ((capture-frame (make-capture-frame)))
       ;; Capture template selection uses org-mks, which insists on
       ;; using a separate window to pick the template. That looks
       ;; weird when we already are making a dedicated frame, so hack
       ;; it to use one window.
       (letf (((symbol-function 'org-switch-to-buffer-other-window)
               (symbol-function 'switch-to-buffer)))
         ,@body))))

(defun my-org-capture-new-frame ()
  "Create a new frame and run org capture, good for slickrun
commands, desktop shortcuts, scripts, etc. For example: \"emacsclientw
-e (my-org-capture-new-frame)\". This ensures that template
selection is presented in the new frame; using org-capture-hook
instead would result in the template selection prompt being
buried in the existing frame.

Returns with the capture frame ready for entry, i.e. not entered
and finalized."
  (interactive)
  (with-capture-frame
   (org-capture)))

(defun org-datetree-find-date-create-month-x (&optional date)
  "Find or create a datetree entry for DATE's year and month.
This is useful when you don't want a heading level for the day,
only for the year and month, as in a workout log that has a table
for the whole month. DATE defaults to today."
  (require 'org-datetree)
  (let* ((date (or date (calendar-gregorian-from-absolute (org-today))))
         (year (nth 2 date))
         (month (car date))
         (day (nth 1 date)))
    (setq-local org-datetree-base-level 1) ; needed to get the buffer to widen right
    (widen)
    (save-restriction
      (goto-char (point-min))
      (org-datetree--find-create
       "^\\*+[ \t]+\\([12][0-9]\\{3\\}\\)\\(\\s-*?\
\\([ \t]:[[:alnum:]:_@#%%]+:\\)?\\s-*$\\)"
       year)
      (org-datetree--find-create
       "^\\*+[ \t]+%d-\\([01][0-9]\\) \\w+$"
       year month)
      (goto-char (prog1 (point) (widen))))))

(defun org-datetree-find-create-here-skip-month-x (&optional date keep-restriction)
  "Calls `org-datetree-find-create-here-x', set to skip the month level."
  (interactive)
  (org-datetree-find-create-here-x date keep-restriction t))

(defun org-datetree-find-create-here-x (&optional date keep-restriction skip-month)
  "Find or create an entry for DATE, which defaults to today.

This is cribbed from `org-datetree-find-date-create', with the
main change being if there is no DATE_TREE property to set the
level of the tree, the current subtree is also searched forward
and backward for a year heading.

If KEEP-RESTRICTION is non-nil, do not widen the buffer. When it
is nil, the buffer will be widened to make sure an existing date
tree can be found.

If SKIP-MONTH is non-nil, no month level will be inserted, only
the year and date."
  (interactive)
  (require 'org-datetree)
  (setq date (or date (calendar-gregorian-from-absolute (org-today))))
  (setq-local org-datetree-base-level 1)
  (or keep-restriction (widen))
  (save-restriction
    (let* ((year-re "^\\*+[ \t]+\\([12][0-9]\\{3\\}\\)\\(\\s-*?\\([ \t]:[[:alnum:]:_@#%]+:\\)?\\s-*$\\)")
           (prop (or
                  ;; Check for a DATE_TREE property in the current
                  ;; subtree. This is scoped so it doesn't trigger on
                  ;; any old datetree at the beginning of the buffer.
                  (save-restriction
                    (save-excursion
                      (outline-back-to-heading)
                      (org-narrow-to-subtree)
                      (org-find-property "DATE_TREE")))

                  ;; next check for a year heading under the current subtree
                  (when (save-restriction
                          (org-narrow-to-subtree)
                          (search-forward-regexp year-re nil t))
                    (when (> (funcall outline-level) 1)
                      (outline-up-heading 1))
                    (point))

                  ;; next search back from point for a year heading
                  (when (search-backward-regexp year-re nil t)
                    (when (> (funcall outline-level) 1)
                      (outline-up-heading 1))
                    (point))

                  ;; Recurse up the tree looking for DATE_TREE properties.
                  ;; (save-restriction
                  ;;   (save-excursion
                  ;;     (catch 'exit
                  ;;       (while (outline-up-heading 1)
                  ;;         (save-restriction
                  ;;           (org-narrow-to-subtree)
                  ;;           (let ((found (org-find-property "DATE_TREE")))
                  ;;             (when found
                  ;;               (throw 'exit found))))))))
                  )))
      (when prop
        (goto-char prop)
        (setq-local org-datetree-base-level
                    (org-get-valid-level (org-current-level) 1))
        (org-narrow-to-subtree)))
    (goto-char (point-min))
    ;(message "searching from %s" (point))
    (let ((year (nth 2 date))
          (month (car date))
          (day (nth 1 date)))
      (org-datetree--find-create
       "^\\*+[ \t]+\\([12][0-9]\\{3\\}\\)\\(\\s-*?\
\\([ \t]:[[:alnum:]:_@#%%]+:\\)?\\s-*$\\)"
       year)
      (if skip-month
          ;; Add a tree for the date under the year, no month level
          (let ((txt (format-time-string "* %Y-%m-%d " (encode-time 0 0 0 day month year)))
                (level (org-get-valid-level (funcall outline-level) 1)))
            (org-end-of-subtree t t)
            (org-back-over-empty-lines)
            (org-paste-subtree level txt))
        (org-datetree--find-create
         "^\\*+[ \t]+%d-\\([01][0-9]\\) \\w+$"
         year month)
        (org-datetree--find-create
         "^\\*+[ \t]+%d-%02d-\\([0123][0-9]\\) \\w+$"
         year month day)))
    (end-of-line))
  (or keep-restriction (save-excursion
                         (org-show-entry)
                         (org-show-subtree))))

;; minutes parsing for workout duration math formulas. Stock table T/t
;; formatting defaults to HH:MM but we need MM:SS.
(defun parse-minutes-x (time)
  "Converts TIME in MM:SS format to a number of minutes."
  (when (string-match "\\([0-9]+\\)\\:\\([0-9]+\\)" time)
    (+ (string-to-number (match-string 1 time))
       (/ (string-to-number (match-string 2 time)) 60.0))))

(defun org-table-from-dough-table ()
  "Converts tables copied and pasted from pizzamaking.com dough
tools to org tables. Works in the current subtree.

http://www.pizzamaking.com/preferment-calculator.html"
  (interactive)
  (save-restriction
    (org-narrow-to-subtree)
    (outline-back-to-heading)

    ;; There are already pipe separators between the different units
    ;; on each line, so we just need to wrap pipes around the first
    ;; and last columns.

    ;; Example:

    ;; Total Formula:
    ;; Flour (100%): 	554.97 g  |  19.58 oz | 1.22 lbs
    ;; Water (82%): 	455.08 g  |  16.05 oz | 1 lbs
    ;; Salt (1.5%): 	8.32 g | 0.29 oz | 0.02 lbs | 1.49 tsp | 0.5 tbsp
    ;; IDY (0.194%): 	1.08 g | 0.04 oz | 0 lbs | 0.36 tsp | 0.12 tbsp
    ;; Oil (1%): 	5.55 g | 0.2 oz | 0.01 lbs | 1.23 tsp | 0.41 tbsp
    ;; Total (184.694%):	1025 g | 36.16 oz | 2.26 lbs | TF = N/A
    ;; Single Ball:	256.25 g | 9.04 oz | 0.56 lbs

    ;; HEADER: SPC MEASURES SPC EOL
    (replace-regexp "^\\([[:alnum:]()% .]+\\):[[:space:]]+\\([[:alnum:] =/.|]+\\) $" "|\\1|\\2|")

    ;; Go back and cycle each table so the columns are aligned.
    ;; org-export won't work on an unaligned table.
    (outline-back-to-heading)
    (while (re-search-forward "^|")
      (org-cycle))))
