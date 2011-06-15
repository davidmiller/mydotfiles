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

;;
;; Projects
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
      (if (string-match (expand-file-name dir) (expand-file-name (buffer-file-name)))
          (setq onzo t)))
    onzo))

;;
;; Infrastructure
;;
;; Commentary:
;;
;; Functions related to the Onzo dev infrastructure - e.g. trac etc
;;

(defun onzo-trac-ticket ()
  "")

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

;;;###autoload
(defun onzo-backend-start (&optional dont-pop)
  "Run the Onzo Backend service in a buffer *onzo-backend*"
  (interactive)
  (onzo-comint-pop "onzo-backend" (concat (onzo-backend-scripts) "/backend_server") nil dont-pop))

;;;###autoload
(defun onzo-backend-stop ()
  "Stop the running instance of the Onzo Backend service if one is running in an Emacs
Buffer."
  (interactive)
  (onzo-subp-stop "onzo-backend"))

;;;###autoload
(defun onzo-backend-restart ()
  "Restart the backend service. Typically because we've made changes to the source"
  (interactive)
  (onzo-backend-stop)
  (run-with-timer 1 nil 'onzo-backend-start (list t)))

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
  (if (onzo-backend-source-p)
      (onzo-file-exists (expand-file-name (concat (first onzo-src-root) "/backend/onzo.conf")))))

;;;###autoload
(defun onzo-backend-running-p ()
  "Predicate determining whether the backend is running"
  (onzo-xp (get-buffer-process "*onzo-backend*")))

;;;###autoload
(defun onzo-backend-db-shell ()
  "Run the Onzo Backend database Shell"
  )

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
(onzo-key "\C-c\C-pb" 'onzo-browser)

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
    ["Launch Onzo shell" onzo-shell]
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