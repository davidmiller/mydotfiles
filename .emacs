;;  Citations
;;
;;     "Show me your ~/.emacs and I will tell you who you are."
;;                                                         [Bogdan Maryniuk]
;;

(require 'cl)

;; All my emacs customisations and packages are under ~/emacs
(defvar emacs-root (expand-file-name "~/emacs")
  "Single point of representation for where our elisp packages
are coming from.")

;;
;; Establish various path / load / require helpers here
;; so we can use them throughout more specific config packages
;;
(defun add-load-dir (root)
  "Add the directories under `root' to load-path"
  (add-to-list 'load-path root)
  (dolist (emacsdir?
           (directory-files root t "^[^\\.]"))
    (when (file-directory-p emacsdir?)
      (add-to-list 'load-path emacsdir?))))

(defun path.join (base &rest paths)
  "Translation of Python's os.path.join. Take path elements and
join them intelligently.

If any element is an abolute path, discard
all previous elements. Otherwise, concatenate base and all paths
elements joined by \."
  (let ((path base))
    (dolist (item paths)
      (if (string= "/" (substring item 0 1))
          (setq path item)
        (if (string= "/" (substring path -1))
            (setq path (concat path item))
          (setq path (concat path "/" item)))))
    path))

(defun emacsdir (path)
  "Concatenate `emacs-root' with `path'."
  (path.join emacs-root path))

(defun sitedir (path)
  "Return a string representing `emacs-root'/site-packages/`path'"
  (path.join emacs-root "site-packages" path))

(defmacro require-many (&rest packages)
  "Require many packages at once. Useful when establishing large lists of
module level requires."
    `(dolist (feature (list ,@packages))
       (require feature)))

;; Add directories for major & minor modes, config directories
(add-load-dir emacs-root)
;; Third party libraries are under ~/emacs/site-packages
(add-load-dir (emacsdir "site-packages"))
;; My Emacs libraries live in ~/emacs/src
(add-load-dir (emacsdir "src"))

;; Load the actual configuration libraries
(require-many 'efuncs 'ecolours 'ekeys 'econf 'elangs)

;; If you're reading this on github, the packages that contain
;; the configurations can be found at http://github.com/davidmiller/emacs/config

