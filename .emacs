;;
;; .emacs
;;
;; If you're reading this on Github, the packages that contain
;; the configurations can be found at http://github.com/davidmiller/emacs/config
;;

;;
;;  Citations
;;
;;     "Show me your ~/.emacs and I will tell you who you are."
;;                                                         [Bogdan Maryniuk]
;;

(require 'cl)

;; All my emacs customisations and packages are under ~/emacs

(defvar win-p (eq 'windows-nt system-type) "Are we on a windows system?")

(defmacro *nix nil "Is this machine a *nix box?" `(not win-p))

(defvar ~ (cond
           (win-p (expand-file-name "/cygwin/home/david/"))
           (t (expand-file-name "~/")))
  "Where do I call home?")

(defvar emacs-root
  (concat ~ "emacs")
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

;;
;; El-get
;;
;; Commentary:
;;
;; El-get is by far the most civilized way to enjoy third party
;; packages for Emacs.
;;
(require 'el-get)
(setq el-get-dir (emacsdir "site-packages"))
(setq el-get-status-file (path.join emacs-root "site-packages" ".status.el"))

(defmacro el-get-hub (&key user &rest sources)
  "Create el-get sources from the symbols passed as `sources'"
  `,@(loop for package in sources
            collect `(:name ,package
                            :type git
                            :url ,(concat
                                    "git@github.com:"
                                    user "/"
                                    (symbol-name package)
                                    ".git"))))

(defmacro defsources (&key github &rest body)
  "Splice the expanded github repos into our form for
creating an `el-get-sources' variable"
  `(setq el-get-sources
         '(,@(apply #'append (loop for repo in github
                          collect (macroexpand
                                   (cons 'el-get-hub repo)))))))

(defsources
  :github
  ;; Firstly let's get my packages
  ((:user "davidmiller"
          emodes pony-mode lintnode come-fly thrift-mode)
   (:user "technomancy"
          ;; Get slime from this github mirror until Clojure
          ;; sort out numerous infrastructure issues
          slime
          clojure-mode)))

(setq my-packages
      (append
       '()
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)

;; third party libraries are under ~/emacs/site-packages
(add-load-dir (emacsdir "site-packages"))

;; Load the actual configuration libraries
(require-many 'efuncs 'ecolours 'ekeys 'econf 'elangs)

;; Code ends
