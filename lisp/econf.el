;; Generic emacs configuration directives

;;; Initialization
(setq inhibit-startup-message t) ;; No more welcome for me
;; Make stuff wander about
(defconst animate-n-steps 10)
(defun emacs-reloaded ()
  (animate-string (concat ";; Initialization successful, welcome to "
                          (substring (emacs-version) 0 16)
                          ". \n;; Loaded with .emacs enabled")
                  0 0)
  (newline-and-indent)  (newline-and-indent))
(add-hook 'after-init-hook 'emacs-reloaded)

(tool-bar-mode nil);; Remove icons from gtk menu
(setq ring-bell-function 'ignore);; disable bell function
(column-number-mode 1);; Enable Colum numbering
(blink-cursor-mode nil) ;; Stop cursor from blinking
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
(setq transient-mark-mode t) ;; Where am I now?
(delete-selection-mode t)
(setq indicate-empty-lines t)
;;;;;;;;;;;;;;;;;;;;;;;;;  Files   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Put backup files (ie foo~) in one place. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "/home/david/tmp/emacs_backups/"))
(setq backup-directory-alist (list (cons "." backup-dir)))
;; Let buffer names be unique in a nicer way
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(setq wdired-allow-to-change-permissions t) ;; Allow perm changing in Dired



;; Editing
(load-library "light-symbol")
(require 'auto-complete-config)
(setq-default ac-sources '(ac-source-words-in-same-mode-buffers
                           ac-source-yasnippet
                           ac-source-filename
                           ac-source-files-in-current-dir))
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(add-hook 'emacs-lisp-mode-hook
          (lambda () (add-to-list 'ac-sources 'ac-source-symbols)))
(add-hook 'python-mode-hook
          (lambda () (add-to-list 'ac-sources 'ac-source-ropemacs)))
(global-auto-complete-mode t)
(ac-css-keywords-initialize)
                                        ;(ac-set-trigger-key "C-c C-/")
                                        ;(setq ac-auto-start nil)
(setq ac-auto-start 2)

(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/emacs/yasnippet/snippets/text-mode")

;;;;;;;;;;;;;;;;;;;;;;;;  Buffer Management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq split-window-preferred-function 'split-window-sensibly)
(setq split-width-threshold 20)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Org" ;; all org-related buffers
                (mode . org-mode))
               ("Profile"    ;; personal config files
                (filename . ".emacs\$"))
               ("Solariffic" ;; Solariffic CRM on local machine
                (filename . "work/solar"))
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
;; Connect to Freenode on C-c e f
;(global-set-key "\C-cef" ;; (lambda () (interactive)
                         ;;   (erc :server "irc.freenode.net" :port "6667"
                         ;;        :nick "davidmiller")))
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
      '(("freenode.net" "#emacs" "#django-mode" "#buildbot" "#celery")))
 (erc-spelling-mode 1)
