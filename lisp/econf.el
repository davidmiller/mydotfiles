;; Initialization successful, welcome to GNU Emacs 23.3.2.
;; Loaded with .emacs enabled

;;; Initialization
(setq inhibit-startup-message t) ;; No more welcome for me
;; Make stuff wander about
(defconst animate-n-steps 10)
(defun emacs-reloaded ()
  (switch-to-buffer "*scratch*")
  (animate-string (concat ";; Initialization successful, welcome to "
                          (substring (emacs-version) 0 16)
                          ". \n;; Loaded with .emacs enabled")
                  0 0)
  (newline-and-indent)  (newline-and-indent))
(add-hook 'after-init-hook 'emacs-reloaded)

(tool-bar-mode nil);; Remove icons from gtk menu
(setq ring-bell-function 'ignore);; disable bell function
(column-number-mode 1);; Enable Colum numbering
;;(blink-cursor-mode nil) ;; Stop cursor from blinkin
(require 'bar-cursor)
(bar-cursor-mode t) ;; Put the cursor on a diet
(defalias 'yes-or-no-p 'y-or-n-p) ;; less typing for me
(display-time) ;; show it in the modeline
(setq-default indent-tabs-mode nil) ;; Spaces instead of tabs
;; Brief aside via Georg Brandl
;;
;; Thus spake the Lord:
;; Thou shalt indent with four spaces. No more, no less.
;; Four shall be the number of spaces thou shalt indent,
;; and the number of thy indenting shall be four.
;; Eight shalt thou not indent, nor either indent thou two,
;; excepting that thou then proceed to four.
;;
;; Tabs are right out.
(setq tab-width 4)

(set-face-attribute 'default nil :height 105)
(show-paren-mode 1) ;; Highlight parenthesis pairs
(setq transient-mark-mode t) ;; Highlight region whenever the mark is active
(delete-selection-mode t) ;; Delete contents of region when we start typing
(setq indicate-empty-lines t)

;;;;;;;;;;;;;;;;;;;;;;;;;  Files   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Put backup files (ie foo~) in one place. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "/home/david/tmp/emacs_backups/"))
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq temporary-file-directory "/tmp/")
;; Let buffer names be unique in a nicer way
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(setq wdired-allow-to-change-permissions t) ;; Allow perm changing in Dired



;; Editing
(load-library "light-symbol")
(require 'autopair)

(require 'auto-complete-config)

;; (setq-default ac-sources '(ac-source-words-in-same-mode-buffers
;;                            ac-source-yasnippet
;;                            ac-source-filename
;;                            ac-source-files-in-current-dir))
(add-to-list 'ac-dictionary-directories "~/emacs/auto-complete/dict")

(ac-config-default)
(add-to-list 'ac-modes 'erlang-mode)
(add-to-list 'ac-modes 'erlang-shell-mode)
(add-to-list 'ac-modes 'thrift-mode)
(setq-default ac-sources (add-to-list 'ac-sources 'ac-dictionary 'ac-source-yasnippet))
(add-hook 'emacs-lisp-mode-hook
          (lambda () (add-to-list 'ac-sources 'ac-source-symbols)))
(add-hook 'auto-complete-mode-hook
          (lambda ()
            ()))
;; (add-hook 'python-mode-hook
;;           (lambda () (add-to-list 'ac-sources 'ac-source-ropemacs)))
(global-auto-complete-mode t)
                                        ;(ac-css-keywords-initialize)
                                        ;(ac-set-trigger-key "C-c C-/")
                                        ;(setq ac-auto-start nil)
(setq ac-auto-start 2)
(setq ac-ignore-case nil)
(setq ac-quick-help-delay 1)
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/emacs/yasnippet/snippets/text-mode")

;;;;;;;;;;;;;;;;;;;;;;;;  Buffer Management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq split-window-preferred-function 'split-window-sensibly)
(winner-mode 1)
;(setq split-width-threshold 50)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Org" ;; all org-related buffers
                (mode . org-mode))
               ("Profile"    ;; personal config files
                (filename . ".emacs\$"))
               ("Onzo" ;; Work related sectioning
                (or (filename . "onzo") ;; Local Machine
                    (filename . "scp:devs@devmac"))) ;; TRAMPing to the dev Mac
               ("Gnus"
                (name . "*Group*"))
               ("Programming" ;; prog stuff not already in MyProjectX
                (or
                 (mode . python-mode)
                 (mode . perl-mode)
                 (mode . ruby-mode)
                 (mode . php-mode)
                 (mode . emacs-lisp-mode)
                 (filename . ".tpl\$")
                 ))
               ("Mail"
                 (or  ;; mail-related buffers
                  (mode . message-mode)
                  (mode . mail-mode)
                  ;; etc.; all your mail related modes
                  ))
               ("Jabber"
                (or
                 (mode . jabber-chat-mode)
                 (mode . jabber-roster-mode)
                 ))
               ("Snippets"
                (filename . "yasnippet/snippets"))
               ("ERC"   (mode . erc-mode))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(setq frame-title-format '(buffer-file-name "%f" ("%b")))

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching


(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode 1))

;; So I'll be lowercasin'
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Shell
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Version Control
(add-to-list 'load-path "/home/david/emacs/dvc/")
(require 'dvc-autoloads)
(setq dvc-tips-enabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Jabber client ;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/emacs/emacs-jabber-0.8.0")
(load "jabber-autoloads")
(setq jabber-account-list
      '(("david@deadpansincerity.com"
         (:network-server . "talk.google.com")
         (:connection-type . ssl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ERC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'erc-mode-hook (lambda () (longlines-mode t)))
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(defmacro erc-connect (command server port nick)
  "Create interactive command `command', for connecting to an IRC server. The
      command uses interactive mode if passed an argument."
  (fset command
        `(lambda (arg)
           (interactive "p")
           (if (not (= 1 arg))
               (call-interactively 'erc)
             (erc :server ,server :port ,port :nick ,nick)))))
(autoload 'erc "erc" "" t)
(erc-connect erc-freenode "irc.freenode.net" 6667 "davidmiller")
(global-set-key "\C-cef" 'erc-freenode)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#pony-mode" "#celery")))
 (erc-spelling-mode 1)

;; Programming - IDE stuff

(require 'smart-operator)
(setq smart-operator-double-space-docs nil)
;; (load-file "~/emacs/cedet/common/cedet.el")
;; ;; Use CEDET projects
;; (global-ede-mode t)
;; (semantic-load-enable-excessive-code-helpers)
;; (require 'semantic-ia)
;; (remove-hook 'python-mode-hook 'wisent-python-default-setup)

;; Modeline customisations
(require 'diminish)
(require 'rainbow-mode)
(eval-after-load "light-symbol" '(diminish 'light-symbol-mode))
(eval-after-load "flymake" '(diminish 'flymake-mode "F"))
(eval-after-load "eldoc" '(diminish 'eldoc-mode "E"))
(eval-after-load "yasnippet" '(diminish 'yas/minor-mode "Y"))
(eval-after-load "autopair" '(diminish 'autopair-mode))
(eval-after-load "smart-operator" '(diminish 'smart-operator-mode))
(eval-after-load "whitespace" '(diminish 'global-whitespace-mode))
(eval-after-load "whitespace" '(diminish 'rainbow-mode))
;; Dictionary
(load "dictionary-init")




;;;;;;;;;;;;;;;;;;;;;;;;;;;; Web browsing stuff here innit ;;;;;;;;;;;;;;;;;;
(setq
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program "chrome")
(global-set-key "\C-cff" 'browse-url)
(load-library "browse-apropos")
;;(setq browse-url-browser-function "firefox")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ORG mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(add-hook 'org-mode-hook '(lambda ()
                            (set-mode-style textual-style)))

;; Testing out remember-mode
(org-remember-insinuate)
(setq org-directory "~/src/onzo/scratch/")
(setq org-default-notes-file "~/src/onzo/scratch/notes.org")
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key global-map "\C-cr" 'org-remember)

(setq org-remember-templates
      '(("Todo" ?t "* TODO %^{Brief Description} %^g\n%?\nAdded: %U"
         "~/notes/organized.org" "Tasks")))

;; Wiki editing
(require 'yaoddmuse)
;; w3m
(require 'w3m-load)
(require 'w3m-e21)
(provide 'w3m-e23)
(setq w3m-default-display-inline-images t)
(setq w3m-use-cookies t)

;; Unittests
(require 'fringe-helper)
(autoload 'test-case-mode "test-case-mode" nil t)
(autoload 'enable-test-case-mode-if-test "test-case-mode")
(autoload 'test-case-find-all-tests "test-case-mode" nil t)
(autoload 'test-case-compilation-finish-run-all "test-case-mode")
(add-hook 'find-file-hook 'enable-test-case-mode-if-test)

;; Session Management
(desktop-save-mode t)

;; Blogging
(require 'weblogger)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(weblogger-config-alist (quote (("deadpansincerity" "http://blog.deadpansincerity.com/xmlrpc.php" "admin" "" "1")))))

;; Function names in the header line
(require 'which-func)
(which-func-mode 1)
 (eval-after-load "which-func"
      '(progn
         (add-to-list 'which-func-modes 'c++-mode)
         (add-to-list 'which-func-modes 'js-mode)
         (add-to-list 'which-func-modes 'lisp-mode)
         (add-to-list 'which-func-modes 'emacs-lisp-mode)
         (add-to-list 'which-func-modes 'python-mode)))
(delete (assoc 'which-func-mode mode-line-format) mode-line-format)
(setq which-func-header-line-format
              '(which-func-mode
                ("" which-func-format
                 )))
(defadvice which-func-ff-hook (after header-line activate)
  (when which-func-mode
    (delete (assoc 'which-func-mode mode-line-format) mode-line-format)
    (setq header-line-format which-func-header-line-format)))
(add-hook 'find-file-hooks 'which-func-ff-hook)

;(require 'onzo)

(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)
(setq ack-executable (executable-find "ack-grep"))


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
(setq package-user-dir (expand-file-name "~/emacs/elpa"))