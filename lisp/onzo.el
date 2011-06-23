;; This file contains functions useful to Onzo Employees working within Emacs

;; Dependencies
(require 'cl)
(require 'easymenu)
(require 'thingatpt)

;;
;; Configuration
;;
;; Commentary:
;;
;; Configuration of onzo.el occurs here. we *could* offer customize options,
;; but seeing as anyone using this is likely to be a programmer, it seems that
;; they can probably handle (setq 'var "value")
;;

(defvar onzo-src-root (list "~/src/onzo" "scp:devs@devmac")
  "This variable stores the parent directories of all your Onzo files. At a later
stage this value can be altered to accept a lisp expression which determines
whether a file is or is not an Onzo file. For now, it's a straight directory
match.

When looking for things like the backend, these values will be taken in the sequence
they appear in the list.
")

(defvar onzo-tracker-ticket-url "http:/trac.onzo.com:8000/ticket/"
  "Stores the url fragment to which ticket numbers will be appended when jumping to
tickets from Emacs.")

(defvar onzo-sseweb-port 8080
  "The port on which you would like to run the sse-web service")

(defvar onzo-sseweb-host "0.0.0.0"
  "The host on which you would like to run the sse-web service")

;;
;; Utilities
;;
;; Commentary:
;;
;; Generic lispish utilities that serve either as abstraction layers or
;; syntax sugar for common idioms.
;;

(defun onzo-xp (expr)
  "Wrap the results of `expr`, evaluating to t or nil when creating predicate-p functions"
  (if expr t nil))

;;
;; Emacs utilities
;;
;; Commentary:
;;
;; These utilities outline various re-usable wrappers and convenience
;; abstractions for common patterns when dealing with the Emacs API
;;

;;;###autoload
(defun onzo-pop (buffer &optional dont-onzo)
  "Wraps pop-to and get buffer for sring `buffer`. Makes sure that keybindings for
Onzo-mode are available to popped buffers (e.g.) sub-processes"
  (pop-to-buffer (get-buffer buffer))
  (if (not dont-onzo)
      (onzo-mode)))

;;;###autoload
(defun onzo-file-exists (path)
  "Return `path` after ascertaining that it exists"
  (if (file-exists-p path)
      path))

;;
;; Sub-Processes
;;
;; Commentary:
;;
;; Re-usable program components for interacting with sub-processes
;; such as servers etc
;;

(defvar onzo-service-hash (make-hash-table)
  "A hash table of all services and their ports")

;;;###autoload
(defun onzo-comint-pop (name command &optional args dont-pop)
  "Make a comint buffer and pop to it."
  (ansi-color-for-comint-mode-on)
  (apply 'make-comint name command args)
  (if (not dont-pop)
      (onzo-pop (concat "*" name "*"))))

;;;###autoload
(defun onzo-subp-stop (name)
  "Check to see if the process named `name` is running and then stop it"
  (let ((proc (get-buffer-process (concat "*" name "*"))))
    (if proc (kill-process proc))))


(defmacro onzo-defservice (name command args &optional port)
  "Expand to be an interactive onzo service e.g. sse/backend/whitelabel
Args are expected to be: `name` `command` `args` `dont-pop`
where name and command are strings, args a list, and dont-pop optional.
"
  (let* ((namestr (symbol-name name))
         (start (concat "onzo-" namestr "-start"))
         (stop (concat "onzo-" namestr "-stop")))
    (if port
        (puthash namestr port onzo-service-hash))
    `(progn
       (defun ,(intern start) ()
         "Start the service"
         (interactive)
         (message "starting...")
         (onzo-comint-pop ,namestr ,command ,args))

       (defun ,(intern stop) ()
         "Stop the service"
         (interactive)
         (message "stopping")
         (onzo-subp-stop ,namestr))

       (defun ,(intern (concat "onzo-" namestr "-restart")) ()
         "Restart the service..."
         (interactive)
         (message "Restarting...")
         (,(intern stop))
         (run-with-timer 1 nil ',(intern start) ))

       (defun ,(intern (concat "onzo-" namestr "-running-p")) ()
         "Determine whether we're running or not"
         (onzp-xp (get-buffer-process ,(concat "*" namestr "*")))))))

;; (onzo-defservice backend (concat (onzo-backend-scripts) "/backend_server") nil 8080)
q

(defmacro onzo-defservice-group (name services)
  "Create a group of services that function as a project together")


;;
;; Onzorifficity
;;
;; Commentary:
;;
;; These functions are used to determine whether a buffer should be an Onzo
;; buffer.
;;

(defun onzo-p ()
  "Determine whether the current buffer is an Onzo project"
  (interactive)
  (let ((onzo nil)
        (onzo-dirs (if (stringp onzo-src-root)
                       (list onzo-src-root)
                     onzo-src-root)))
    (dolist (dir onzo-dirs)
      (if (string-match (expand-file-name dir)
                        (expand-file-name (buffer-file-name)))
          (setq onzo t)))
    onzo))

;;
;; Infrastructure
;;
;; Commentary:
;;
;; Functions related to the Onzo dev infrastructure - e.g. trac etc
;;

;;;###autoload
(defun onzo-ticket (num)
  "Jump to ticket number `num` on Onzo's bug tracker"
  (interactive "sTicket Number: ")
  (browse-url (concat onzo-tracker-ticket-url num)))

;; TODO
;; Jump to trac locations - SEK/ZBD/SW releases - probably
;; interactive defun with string mapping of choices

;;
;; Backend
;;
;; Commentary:
;;
;; We would like to be able to run the backend server with an automatic reloader.
;; This is the More Civilised (TM) way to go about things.
;;

;;;###autoload
(defun onzo-backend-scripts ()
  "Locate the onzo backend/lib dir beneath onzo-src-root.
For now, this function assumes that you will have `onzo-src-root`/backend as
the location of your local backend repo."
  (let((backend? (expand-file-name (concat (first onzo-src-root) "/backend/lib"))))
    (if (file-exists-p backend?)
        backend?)))

(onzo-defservice backend (concat (onzo-backend-scripts) "/backend_server") nil)

;;;###autoload
(defun onzo-backend-reload ()
  "Check to make sure this is an Onzo source file for the Backend service, and that the
service is running. if so, restart it.

Used as an after-save hook."
  (if (and
       (onzo-p)
       (onzo-backend-source-p))
      (progn
        (message "Reloading Backend")
        (onzo-backend-restart))))

;;;###autoload
(defun onzo-backend-source-p ()
  "Predicate to determine whether the current file is an Onzo Backend service source file"
  (onzo-xp (string-match-p (expand-file-name (concat (first onzo-src-root) "/backend"))
                      (expand-file-name buffer-file-name))))

;;;###autoload
(defun onzo-backend-onzo-dot-conf ()
  "Return the path to the relevant onzo.conf for the backend."
  (onzo-file-exists (expand-file-name (concat (first onzo-src-root) "/backend/onzo.conf"))))

;;;###autoload
(defun onzo-.conf ()
  "Locate and open backedn/onzo.conf"
  (interactive)
  (find-file (onzo-backend-onzo-dot-conf)))


;; TODO
;;;###autoload
(defun onzo-backend-db-shell ()
  "Run the Onzo Backend database Shell"
  )

;; TODO
;;;###autoload
(defun onzo-console ()
  "Run the onzo console for this backend")

;;
;; SSE
;;
;; Commentary:
;;
;; Support handling of the SSE web layer - understand code paths and
;; basic management commands.
;;
;; Deep django integration is better done elsewhere (e.g. pony-mode)
;; but the minimum to run the whole sse service with one M-x command
;; is worthwhile to have a package that doesn't require external dependencies.
;;
;; Configuration:
;;
;; `onzo-sseweb-port` Port to run onzo-sse on (defaults to 8000)
;; `onzo-sseweb-host` Host to run onzo-sse from (defaults to 0.0.0.0)
;;

;; TODO
;;;###autoload
(defun onzo-sse-root ()
  "Get the root of our local sseweb instance")

(onzo-defservice sse (concat (onzo-sse-root) "/onzo_pss/manage.py")
                 ("runserver"
                  (concat"http://" onzo-sseweb-host ":" onzo-sseweb-port)
                  dont-pop)))

;;
;; Data warehouse
;;
;; Commentary:
;;
;; Run the data warehouseing script as a service
;;
(onzo-defservice data-thrift
                 (expand-file-name (concat
                                    (first onzo-src-root)
                                    "/data_warehouse/scripts/runserver.py"))
                 nil 30303)

(onzo-defservice data-frontend
                 (expand-file-name (concat
                                    (first onzo-src-root)
                                    "/data_warehouse/scripts/runfrontend.py"))
                 nil 4567)


;;
;; Hollism
;;
;; Commentary:
;;
;; Provide support for single-command starting/reloading, and switching between
;; active projects -e.g. M-x onzo-whitelabel should understand the relevant
;; components, and run them.
;;
;; TODO: Add support for arbitrary user projects or project customizations - e.g.
;; adding arbitrary lists of shell commands to be execurted as one 'project'
;;

(defun onzo-sse ()
  "Run the onzo sse service - e.g. the backend and sse-web"
  (interactive)
  (onzo-backend-restart))



;;
;; Firmware
;;
;; Commentary:
;;
;; Provide convenience helpers for working with Firmware
;;

;;;###autoload
(defun onzo-yy-mm-dd ()
  "Extract the date values as integers and retutn in a list"
  (interactive)
  (let ((date-re (concat "\\([0-9]\\{2\\}\\)-"
                            "\\([0-9]\\{2\\}\\)-"
                            "\\([0-9]\\{2\\}\\)")))
  (if (thing-at-point-looking-at date-re)
      (list (match-string 1) (match-string 2) (match-string 3)))))


;;;###autoload
(defun onzo-sek-version ()
  "Get the SEK version from the date at point"
  (interactive)
  (let ((date (onzo-yy-mm-dd)))
    (message date)
    (if (and date (find-if-not #'null date))
        (message "Failed to extract date from word at point")
      (message "%d" (+ (lsh (first date) 9)
                       (lsh (second data) 5)
                       (third data))))))

;;
;; Minor Mode
;;
;; Commentary:
;;
;; Provide some keybindings and a menu for the Onzo functionality
;;


(defun onzo-key(binding function)
  "Bind function to binding in onzo-minor-mode-map"
  (define-key onzo-minor-mode-map binding function))

(defvar onzo-minor-mode-map
  (let ((map (make-keymap)))
    map))
(onzo-key "\C-c\C-ot" 'onzo-ticket)
(onzo-key "\C-c\C-o." 'onzo-.conf)

;; Menu
;;
;; Commentary:
;;
;; This will only work OOTB for emacs >=19 due to the dependency on easymenu
;; but frankly, that's fine with me.

(defvar onzo-menu nil
  "The menu for Onzo mode.")
(easy-menu-define
  onzo-menu onzo-minor-mode-map "Onzo Mode Menu"
  '("Onzo"
    ;; Interactive
    ["Jump to ticket" onzo-ticket]
    ))

(defvar onzo-minor-mode-hook nil)

(define-minor-mode onzo-minor-mode
  "Onzoriffic"
  :initial nil
  :lighter " Onzo"
  :keymap onzo-minor-mode-map)

(defun onzo-mode()
  "Initialize Onzo mode"
  (interactive)
  (onzo-minor-mode t)
  (run-hooks 'onzo-minor-mode-hook))

;;  Check for Onorifficity when opening files
(add-hook 'find-file-hook (lambda ()
                            (if (onzo-p)
                                (onzo-mode))))

(add-hook 'after-save-hook (lambda () (onzo-backend-reload)))

(provide 'onzo)
;; onzo.el ends