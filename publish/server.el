;; A static site build server for writing notes in orgmode with hot reloading in
;; the browser.
;;
;; When any .org files in notes/ are saved, the site is built in writing
;; mode, which includes JS in the pages that sets up a WebSocket to receive
;; reload messages.
;;
;; This is a rewrite of server.rb for direct use in Emacs, written as an
;; exercise coding with ChatGPT. ChatGPT generated the bones of the buffer save
;; hook, process sentinel, and WebSockets structure. Generated code mostly
;; worked, although I had to debug and fix hallucinated WebSockets hooks
;; signatures. Effort was about 1/3 chat and testing to bootstrap the program,
;; 1/3 debugging hallucinations and a connect hook misname (by me), 1/3
;; refactoring and rewriting comments in my style.
;;
;; Usage:
;;   M-x notes-server-start
;;   open http://127.0.0.1:5000/notes
;;   C-x C-f notes/xyz.org
;;   C-x C-s
;;
;; Requirements:
;; - (package-install 'websocket)
;;
;; TODO:
;; - Normalize and namespace all functions and variables
;; - Debug/log the background process
;; - Build images and styles

(require 'cl) ; lexical-let
(require 'websocket)
(require 'simple-httpd)

(setq notes-root-dir (file-name-directory
                       (directory-file-name
                        (file-name-directory (if load-in-progress
                                                 load-file-name
                                               (buffer-file-name))))))

(defvar notes-publish-process nil
  "Tracks the currently running org-publish background process. Used to
prevent concurrent builds if files are saved in quick succession.")

(defvar notes-publish-pending nil
  "The last buffer file saved during a running build, if any.")

(defvar notes-websocket-server nil)

(defvar notes-websocket-connections '()
  "List to active WebSocket connections for broadcasting rebuild completed events.")

(defun notes-websocket-open (websocket)
  "Callback to register active WebSocket connections."
  (add-to-list 'notes-websocket-connections websocket))

(defun notes-websocket-close (websocket)
  "Callback to unregister closed WebSocket connections."
  (setq notes-websocket-connections (remove websocket notes-websocket-connections)))

(defun notes-websocket-error (websocket sym)
  (message "WebSocket error [%s]: %s" websocket sym))

(defun notify-all-websocket-clients (message)
  "Sends MESSAGE to all WebSocket clients."
  (dolist (websocket notes-websocket-connections)
    (when (websocket-openp websocket)
      (websocket-send-text websocket message))))

(defun rebuild-site-completed (buffer-file)
  ;; Notify clients to reload
  (let ((msg (json-encode `((:type . "build_complete") (:source . ,buffer-file)))))
    (notify-all-websocket-clients msg))

  ;; Clean up process state
  (setq notes-publish-process nil)

  ;; Immediately rebuild any pending file changes
  (when notes-publish-pending
    (let ((pending notes-publish-pending))
      (setq notes-publish-pending nil)
      (rebuild-site pending))))

(defun make-rebuild-site-sentinel (buffer-file)
  (lexical-let ((buffer-file buffer-file))
    (lambda (process event)
      (cond ((string= event "finished\n")
             (rebuild-site-completed buffer-file))
            (t
             (message "Ignoring process event [%s]: %s" process event))))))

(defun rebuild-site (buffer-file)
  (message "Rebuilding for file %s" buffer-file)
  (let ((default-directory notes-root-dir))
    (setq notes-publish-process
          (start-process "notes-publish" "*notes-publish*" "make" "writing=t")))
  (set-process-sentinel notes-publish-process (make-rebuild-site-sentinel buffer-file)))

(defun notes-project-buffer-save-hook ()
  "Watches for project file changes and triggers a rebuild.
TODO: Watch *.css and publish.el."
  (let ((buffer-file (buffer-file-name)))
    (when (and buffer-file
               (string-prefix-p (expand-file-name "content/notes" notes-root-dir)
                                (file-name-directory (buffer-file-name)))
               (string-match "\\.org$" buffer-file))

      (if (and notes-publish-process (process-live-p notes-publish-process))
          (progn
            (message "Buffer saved: %s. Queueing rebuild." buffer-file)
            (setq notes-publish-pending buffer-file))
        (rebuild-site buffer-file)))))

(defun notes-server-start ()
  "Starts the project static file web server and WebSocket server.
Installs a save hook to watch for project file changes and trigger a
rebuild."
  (interactive)
  (let ((httpd-port 5000))
    (httpd-serve-directory (expand-file-name "build" notes-root-dir)))

  (setq notes-websocket-server (websocket-server 5001
                                                  :on-open 'notes-websocket-open
                                                  :on-close 'notes-websocket-close
                                                  :on-error 'notes-websocket-error))

  (setq notes-publish-pending nil)

  (add-hook 'after-save-hook 'notes-project-buffer-save-hook))

(defun notes-server-stop ()
  "Stop the static file server."
  (interactive)
  (remove-hook 'after-save-hook 'my-buffer-save-hook)

  (when notes-websocket-server
    (websocket-server-close notes-websocket-server)
    (setq notes-websocket-server nil))

  (httpd-stop))

;;(global-set-key (kbd "C-c s") 'start-static-file-server)
;;(global-set-key (kbd "C-c q") 'stop-static-file-server)

(when nil
  (let ((buffer-file "xyz.org"))
    (json-encode `((:type . "build_complete") (:source . ,buffer-file))))
  )
