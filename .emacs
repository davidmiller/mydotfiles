;;  Citations
;;
;;     "Show me your ~/.emacs and I will tell you who you are."
;;                                                         [Bogdan Maryniuk]
;;

(require 'cl)

;; All my emacs customisations and packages are under ~/emacs
(defvar emacs-root (expand-file-name "~/emacs"))

(defun add-load-dir (root)
  "Add the directories under `root' to load-path"
  (add-to-list 'load-path root)
  (dolist (emacsdir?
           (directory-files root t "^[^\\.]"))
    (when (file-directory-p emacsdir?)
      (add-to-list 'load-path emacsdir?))))

(add-load-dir emacs-root)
(dolist (config (list "econf" "ekeys" "ecolours" "efuncs" "elangs"))
  (load-library config))

;; If you're reading this on github, the libraries that get loaded above
;; are in the emacs branch of this repo in the lisp/ dir



