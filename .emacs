;;
;; .emacs
;;
;; If you're reading this on Github, there is much deeper configuration to be
;; found at http://github.com/davidmiller/emacs/ - particularly config/
;;

;;
;;  Citations
;;
;;     "Show me your ~/.emacs and I will tell you who you are."
;;                                                         [Bogdan Maryniuk]
;;

;; We'll be using cl idioms quite a lot so let's get that out of the way early.
(require 'cl)

;;
;; Lispicised idioms from other places. These should naturally be in
;; emacs/config/efuncs.el but get used too early in the init process
;;
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

(defun chomp (str)
  "Equivalent to Perl's Chomp.
Remove leading and tailing whitespace

Gleefully stolen from: www.emacswiki.org/emacs/ElispCookbook"
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))


;;
;; Top level platform determinism: Establish what kind of world we're in.
;;
(defvar win-p (eq 'windows-nt system-type) "Are we on a windows system?")

(defvar *nix (not win-p) "Is this machine a *nix box?")

(defmacro ifnix (nixicised alternative)
  "Check to see if we're on some kind of *nix platform. If we are, do
NIXICISED if not, do ALTERNATIVE."
  `(cond
    (*nix ,nixicised)
    (win-p ,alternative)
    (t (error "Unknown platform type - have you set *nix/win-p ?"))))

(defvar hostname (ifnix
                  (chomp (shell-command-to-string "hostname"))
		  "TODO")
  "Holds this machine's hostname.")

(defmacro* ifhost (host then &optional (else nil else-p))
  "If the value of `hostname' and HOST are equal, perform THEN otherwise ELSE."
  `(if (string-equal hostname ,(symbol-name host))
       ,then
     ,(if else-p else)))

(defvar ~ (ifnix
           (expand-file-name "~/")
           (expand-file-name "c:/Users/David/"))
  "Where do I call home?")

;;
;; Establish various path / load / require helpers here
;; so we can use them throughout more specific config packages
;;

(defvar emacs-root
  (concat ~ "emacs")
  "Single point of representation for where our elisp packages
are coming from.")

(defun add-load-dir (root)
  "Add the directories under `root' to load-path"
  (add-to-list 'load-path root)
  (dolist (emacsdir?
           (directory-files root t "^[^\\.]"))
    (when (file-directory-p emacsdir?)
      (add-to-list 'load-path emacsdir?))))

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

;; third party libraries are under ~/emacs/site-packages
(add-load-dir (emacsdir "site-packages"))

;; Load the actual configuration libraries
(require-many 'efuncs 'ekeys 'eget 'ecolours 'econf 'elangs)

;; Code ends
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((test-case-name . twisted\.test\.test_memcache) (test-case-name . twisted) (pony-settings make-pony-project :python "~/v/sse/bin/python") (pony-settings make-pony-project :python "/home/david/v/wl/bin/python" :settings "settings")))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(rst-level-1-face ((t (:background "black" :foreground "brown3" :weight bold))) t)
 '(rst-level-2-face ((t (:background "black" :foreground "white" :weight bold)))))
