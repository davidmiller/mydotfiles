;; .emacs Configuration file synced across boxes via Python & github

;;;;;;;;;;;;;;;;;;;;;;;;;;    .emacs    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Reload this file 
(defun x-reload-dot-emacs()
  (interactive)
  (load-file "~/.emacs"))
(defun x-edit-dot-emacs()
  (interactive)
  (find-file "~/.emacs"))
(global-set-key "\C-c\C-r" 'x-reload-dot-emacs)
(global-set-key "\C-c\C-e" 'x-edit-dot-emacs)

;; ;;;;;;;;;;;;;;;;;;;;;;;;; Emacs General ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(set-face-attribute 'default nil :height 100)
(show-paren-mode 1) ;; Highlight parenthesis pairs
(setq transient-mark-mode t) ;; Where am I now?
(add-to-list `load-path "~/.emacs.d/")

;;;;;;;;;;;;;;;;;;;;;;;;; Auto Completion & snippeting ;;;;;;;;;;;;;;

;; Auto Complete
(add-to-list `load-path "~/.emacs.d/auto-complete")
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
;(ac-set-trigger-key "C-c C-/")
;(setq ac-auto-start nil)
(setq ac-auto-start 3)

;;;;;;;;;;;;;;;;   Yasnippet    ;;;;;;;;;;;;;;;
(add-to-list `load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet/snippets")

;;;;;;;;;;;;;;;;;;;;;;;;  Shell   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(global-set-key "\C-c\C-t" 'ansi-term)

;;;;;;;;;;;;;;;;;;;;;;  Bookmarks   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq 
 bookmark-default-file "~/.emacs.d/bookmarks" ; Keep ~/ clean
 bookmark-save-flag 1)                        ; autosave changes

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


;;;;;;;;;;;;;;;;;;;;;;;;  Buffer Management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Via http://www.stringify.com/2006/apr/24/rename/
(defun rename-current-file-or-buffer ()
  (interactive)
  (if (not (buffer-file-name))
      (call-interactively 'rename-buffer)
    (let ((file (buffer-file-name)))
      (with-temp-buffer
        (set-buffer (dired-noselect file))
        (dired-do-rename)
        (kill-buffer nil))))
  nil)
(global-set-key "\C-cR" 'rename-current-file-or-buffer)



(setq ibuffer-saved-filter-groups
  (quote (("default"      
            ("Org" ;; all org-related buffers
              (mode . org-mode))  
            ("Profile"    ;; personal config files
             (filename . ".emacs\$"))
            ("Programming" ;; prog stuff not already in MyProjectX
              (or
                (mode . python-mode)
                (mode . perl-mode)
                (mode . php-mode)
                (mode . emacs-lisp-mode)
                (mode . nxhtml-mode)
                (filename . ".tpl\$")
                ;; etc
                )) 
            ;; ("Mail"
            ;;   (or  ;; mail-related buffers
            ;;    (mode . message-mode)
            ;;    (mode . mail-mode)
            ;;    ;; etc.; all your mail related modes
            ;;    ))
            ("Jabber"
             (or
              (mode . jabber-chat-mode)
              (mode . jabber-roster-mode)
              ))
            ("Twitter" ;; Twitter stuff together
             (name . "Twit"))
            ("Snippets"
              (filename . "yasnippet/snippets"))            
            ("ERC"   (mode . erc-mode))))))
 
;(add-to-list 'ibuffer-never-show-regexps "jpg")
;(add-to-list 'ibuffer-never-show-regexps "jpg$")

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")))



(global-set-key "\C-x\C-b" 'ibuffer) ;; Buffer management
;; File or  buffer name in title
(setq frame-title-format '(buffer-file-name "%f" ("%b"))) 


;; ;; Functions for nice buffer switching with C-tab keybindings
;; (defun next-user-buffer ()
;;   "Switch to the next user buffer in cyclic order.\n
;; User buffers are those not starting with *."
;;   (interactive)
;;   (next-buffer)
;;   (let ((i 0))
;;     (while (and (string-match "^*" (buffer-name)) (< i 50))
;;       (setq i (1+ i)) (next-buffer) )))
;; (global-set-key [C-tab] 'next-user-buffer)

;; (defun previous-user-buffer ()
;;   "Switch to the previous user buffer in cyclic order.\n
;; User buffers are those not starting with *."
;;   (interactive)
;;   (previous-buffer)
;;   (let ((i 0))
;;     (while (and (string-match "^*" (buffer-name)) (< i 50))
;;       (setq i (1+ i)) (previous-buffer) )))
;; (global-set-key [backtab] 'previous-user-buffer)


(iswitchb-mode 1)
(add-to-list 'iswitchb-buffer-ignore "*Messages*")
(add-to-list 'iswitchb-buffer-ignore "*Completions*")
(add-to-list 'iswitchb-buffer-ignore "*Pymacs*")


(defun iswitchb-local-keys ()
  (mapc (lambda (K) 
	      (let* ((key (car K)) (fun (cdr K)))
            (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
	    '(("<right>" . iswitchb-next-match)
	      ("<left>"  . iswitchb-prev-match)
	      ("<up>"    . ignore             )
	      ("<down>"  . ignore             ))))

(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;;;;;;;;;;;;;;;;;;;    Window System    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Don't background emacs when running outside terminal
(when window-system
  (global-unset-key "\C-x\C-z"))

;;;;;;;;;;;;;;;;;;;    Colors    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; let there be lighter times
(defun clarity()
  (interactive)
  (set-face-background 'hl-line "gray")
  (set-background-color "black")
  (set-foreground-color "white")
  (set-string-color "green")
)

(global-hl-line-mode t) ;; Highlights the current line
(set-face-background 'hl-line "gray")

(set-background-color "white")
(set-foreground-color "black")
(set-border-color "white")
(set-cursor-color "dark orange")
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(font-lock-builtin-face ((t (:foreground "dark violet"))))
 '(font-lock-comment-face ((((class color)) (:foreground "red"))))
 '(font-lock-constant-face ((t (:foreground "DodgerBlue4"))))
 '(font-lock-doc-face ((t (:foreground "LightSalmon"))))
 '(font-lock-function-face ((((class color)) (:foreground "dark goldenrod"))))
 '(font-lock-function-name-face ((t (:foreground "Peru"))))
 '(font-lock-keyword-face ((((class color)) (:foreground "sienna"))))
 '(font-lock-string-face ((((class color)) (:foreground "green4"))))
 '(font-lock-type-face ((((class color)) (:foreground "blue"))))
 '(font-lock-variable-face ((((class color)) (:foreground "purple4"))))
 '(font-lock-variable-name-face ((t (:foreground "purple4"))))
 '(font-lock-warning-face ((t (:bold t :foreground "black" :weight bold))))
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) (:background "black"))))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background dark)) (:background "Black"))))
 '(yas/field-highlight-face ((t (:background "light gray")))))


;;;;;;;;;;;;;;;;;;;    Keybindings    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
;(define-key my-keys-minor-mode-map (kbd "C-c C-f") 'insert-function)

(define-minor-mode my-keys-minor-mode
  "A minor mode for overriding keybindings"
  t " my-keys" 'my-keys-minor-mode-map)
(my-keys-minor-mode 1)

(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

;;;;;;;;;;;;;;;;;;;;;    Languages    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/major-modes")

(require 'javascript-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))

;;;;  Lisp

;SLIME interaction
(setq inferior-lisp-program "clisp")
(add-to-list 'load-path "/usr/share/emacs22/site-lisp/slime")
(require 'slime)
(slime-setup)
(global-font-lock-mode t)
(show-paren-mode 1)
(add-hook 'lisp-mode-hook '(lambda ()
      (local-set-key (kbd "RET") 'newline-and-indent)))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

;;;;  PHP
(load-library "php-mode")
;; Smarty
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . html-mode))

;;;;  Python
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(add-hook 'python-mode-hook
          '(lambda () (eldoc-mode 1)) t)
;;;  Djangoriffic
(defun django-time()
  (interactive)
  (add-to-list 'auto-mode-alist '("\\.html\\'" . django-mode)))
(require 'django-mode)


;;;;;;;;;;;;;; Custom set stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(c-offsets-alist (quote ((brace-list-intro . 0) (label . 0))))
 '(mumamo-submode-indent-offset 2)
 '(org-agenda-files nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Jabber client ;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/emacs-jabber-0.8.0")
(load "jabber-autoloads")
(setq jabber-account-list
      '(("david@deadpansincerity.com"
         (:network-server . "talk.google.com")
         (:connection-type . ssl))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ERC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connect to Freenode on C-c e f
(global-set-key "\C-cef" (lambda () (interactive)
                           (erc :server "irc.freenode.net" :port "666"
                                :nick "thatdavidmiller")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;; Twitter ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/twit")  ; Save directory

;; Define M-x commands

(autoload 'twit-show-recent-tweets	"twit" "" t) ; most recent direct tweets (!)
(autoload 'twit-show-at-tweets		"twit" "" t) ; directed to you
(autoload 'twit-show-friends 		"twit" "" t) ; your friends
(autoload 'twit-show-followers 		"twit" "" t) ; your followers

(autoload 'twit-follow-recent-tweets	"twit" "" t) ; at idle, check at background

(autoload 'twit-post			"twit" "" t)
(autoload 'twit-post-region		"twit" "" t)
(autoload 'twit-post-buffer		"twit" "" t)
(autoload 'twit-direct			"twit" "" t) ; tweet to person

(autoload 'twit-add-favorite		"twit" "" t) ; Add to favourite: (*) star
(autoload 'twit-remove-favorite 	"twit" "" t)

(autoload 'twit-add-friend  		"twit" "" t) ; follow a friend
(autoload 'twit-remove-friend 		"twit" "" t) ; emove a frienda

;; Customize twit-multi-accounts in order to use these: ((user . pass) ...)
(autoload 'twit-switch-account 		"twit" "" t)
(autoload 'twit-direct-with-account  	"twit" "" t)
(autoload 'twit-post-with-account 	"twit" "" t)

(autoload 'twit-show-direct-tweets-with-account "twit" "" t)
(autoload 'twit-show-at-tweets-with-account 	"twit" "" t)

(setq twit-user "thatdavidmiller")
(setq twit-show-user-images t)
(setq twit-user-image-dir "~/.emacs.d/twit/")
;; Key bindings examples
;; Requires that autoloads above have been added to ~/.emacs

(global-set-key "\C-cTT"  'twit-follow-recent-tweets) ; (s)how (T)weets
(global-set-key "\C-cTst" 'twit-follow-recent-tweets) ; (s)how (t)weets
(global-set-key "\C-cTsa" 'twit-show-at-tweets)       ; (s)how (a)t
(global-set-key "\C-cTsf" 'twit-show-at-tweets)       ; (s)how (f)riends
(global-set-key "\C-cTsl" 'twit-show-at-tweets)       ; (s)how fo(l)lowers

(global-set-key "\C-cTpp" 'twit-post)		      ; (p)ost
(global-set-key "\C-cTpr" 'twit-post-region)	      ; (p)post (r)egion
(global-set-key "\C-cTpb" 'twit-post-buffer)	      ; (p)post (b)uffer
(global-set-key "\C-cTpr" 'twit-direct)		      ; (p)post (d)irect
(global-set-key "\C-cTfa" 'twit-add-favorite)	      ; (f)avorite (a)dd
(global-set-key "\C-cTfr" 'twit-remove-favorite)      ; (f)avorite (r)emove

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Web browsing stuff here innit ;;;;;;;;;;;;;;;;;;
(setq
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program "firefox") 
(global-set-key "\C-cff" 'browse-url-firefox)
;;(setq browse-url-browser-function "firefox")

;; So I'll like toootally be lowercasin' kthxbai
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ORG mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

;; Testing out remember-mode
(org-remember-insinuate)
(setq org-directory "~/notes/")
(setq org-default-notes-file "~/.notes")
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key global-map "\C-cr" 'org-remember)

(setq org-remember-templates
      '(("Todo" ?t "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" 
         "~/notes/organized.org" "Tasks")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Icicles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/icicles")
(require 'icicles)
(require 'icicles-iswitchb)
(iswitchb-default-keybindings)
(icy-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;  Keyboard Macros  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fset 'cp-line ;; Totally copying the current line
   "\C-e\C-a\C-k\C-y\C-e\C-j\C-y")
