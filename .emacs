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

(defun reload-gnus()
  (interactive)
  (load-file "~/.gnus"))
(global-set-key "\C-cGR" 'reload-gnus)

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
(set-face-attribute 'default nil :height 105)
(show-paren-mode 1) ;; Highlight parenthesis pairs
(setq transient-mark-mode t) ;; Where am I now?
(add-to-list `load-path "~/.emacs.d/")

;;;;;;;;;;;;;;;;;;;;;;;;; Auto Completion & snippeting ;;;;;;;;;;;;;;

;; Auto Complete
(add-to-list `load-path "~/.emacs.d/auto-complete")
(require 'auto-complete)
(require 'auto-complete-config)
(setq-default ac-sources '(ac-source-words-in-same-mode-buffers))
(add-hook 'emacs-lisp-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-symbols)))
(add-hook 'auto-complete-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-filename)))
(global-auto-complete-mode t)
(ac-css-keywords-initialize)
                                        ;(ac-set-trigger-key "C-c C-/")
                                        ;(setq ac-auto-start nil)
(setq ac-auto-start 2)

;;;;;;;;;;;;;;;;   Yasnippet    ;;;;;;;;;;;;;;;
(add-to-list `load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet/snippets/text-mode")


;; (defun yas-templatise ()
;;   (interactive)
;;   (insert "new-file-tpl") ;; Adds the trigger text
;;   (yas/expand))           ;; Trigger snippet expansion

;; ;; Lets list the modes that we want to add templates for
;; (setq yas-templatise-modes '(html-mode-hook
;;                              python-mode-hook
;;                              perl-mode-hook))

;; (while yas-templatise-modes             ;; Loop through that list
;;   (add-hook (car yas-templatise-modes) 'yas-templatise) ;; Add the hook
;;   (setq yas-templatise-modes (cdr yas-templatise-modes))) ;; re-point our mode-list

;(dolist add-hook ('html-mode-hook) 'yas-templatise)
;(add-hook 'html-mode-hook 'yas-templatise)

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
                 (mode . nxhtml-mode)
                 (filename . ".tpl\$")
                 ;; etc
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
(add-to-list 'iswitchb-buffer-ignore "*fsm-debug*")
(add-to-list 'iswitchb-buffer-ignore "*Ibuffer*")
(add-to-list 'iswitchb-buffer-ignore "irc.freenode.net:6667")


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



(require 'color-theme)
(defun color-theme-bluebulator ()
  (interactive)
  (color-theme-install
   '(color-theme-bluebulator
      ((background-color . "#0f0f0f")
      (background-mode . light)
      (border-color . "#191919")
      (cursor-color . "#43667f")
      (foreground-color . "#c9c9c5")
      (mouse-color . "black"))
     (fringe ((t (:background "#191919"))))
     (mode-line ((t (:foreground "#a3a3a3" :background "#163e60"))))
     (region ((t (:background "#29383d"))))
     (font-lock-builtin-face ((t (:foreground "#ccaa61"))))
     (font-lock-comment-face ((t (:foreground "brown"))))
     (font-lock-function-name-face ((t (:foreground "#197db8"))))
     (font-lock-keyword-face ((t (:foreground "#508195"))))
     (font-lock-string-face ((t (:foreground "#6ea07f"))))
     (font-lock-type-face ((t (:foreground"#539355"))))
     (font-lock-variable-name-face ((t (:foreground "#7f989f"))))
     (minibuffer-prompt ((t (:foreground "#a2c4d8" :bold t))))
     (font-lock-warning-face ((t (:foreground "Red" :bold t))))
     )))
(provide 'color-theme-bluebulator)
(color-theme-bluebulator)


;; (global-hl-line-mode t) ;; Highlights the current line
;; (set-face-background 'hl-line "gray")

;; ;(set-background-color "white")
;; (set-background-color "#0f0f0f")
;; ;(set-foreground-color "black")
;; (set-foreground-color "#c9c9c5")
;; ;(set-border-color "white")
;; (set-border-color "#191919")
;; ;(set-cursor-color "dark orange")
;; (set-cursor-color "#43667f")
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;; ; '(font-lock-builtin-face ((t (:foreground "dark violet"))))
;;  '(font-lock-builtin-face ((t (:foreground "#ccaa61"))))
;; ; '(font-lock-comment-face ((((class color)) (:foreground "red"))))
;;  '(font-lock-comment-face ((((class color)) (:foreground "#384347"))))
;; ; '(font-lock-constant-face ((t (:foreground "DodgerBlue4"))))
;; ; '(font-lock-doc-face ((t (:foreground "LightSalmon"))))
;;  '(font-lock-function-face ((((class color)) (:foreground "dark goldenrod"))))
;; ; '(font-lock-function-name-face ((t (:foreground "Peru"))))
;;  '(font-lock-function-name-face ((t (:foreground "#197db8"))))
;; ; '(font-lock-keyword-face ((((class color)) (:foreground "sienna"))))
;;  '(font-lock-keyword-face ((((class color)) (:foreground "#508195"))))
;; ; '(font-lock-string-face ((((class color)) (:foreground "green4"))))
;;  '(font-lock-string-face ((((class color)) (:foreground "#6ea07f"))))
;; ; '(font-lock-type-face ((((class color)) (:foreground "blue"))))
;;  '(font-lock-type-face ((((class color)) (:foreground "#539355"))))
;; ; '(font-lock-variable-face ((((class color)) (:foreground "purple4"))))
;;  ;'(font-lock-variable-name-face ((t (:foreground "purple4"))))
;;  '(font-lock-variable-name-face ((t (:foreground "#7f989f"))))
;;  '(font-lock-warning-face ((t (:bold t :foreground "black" :weight bold))))
;;  '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) (:background "black"))))
;;  '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background dark)) (:background "Black"))))
;;  '(yas/field-highlight-face ((t (:background "light gray")))))




;      (setq background-mode 'light)
 ;     (setq mouse-color "black")
     ;; (fringe ((t (:background "#191919"))))
     ;; (mode-line ((t (:foreground "#a3a3a3" :background "#163e60"))))
     ;; (region ((t (:background "#29383d"))))
     ;; (minibuffer-prompt ((t (:foreground "#a2c4d8" :bold t))))
     ;; (font-lock-warning-face ((t (:foreground "Red" :bold t))))





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
(add-hook 'javascript--mode-hook '(lambda ()
                             ('yas/minor-mode)))


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
;(load-library "php-mode")
(load "/home/david/.emacs.d/nxhtml/autostart.el")
;; Smarty
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . html-mode))

;;;;  Python
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
                                        ;(add-hook 'python-mode-hook
                                        ;         '(lambda () (eldoc-mode 1)) t)
(add-to-list 'auto-mode-alist '("\\.wsgi\\'" . python-mode))  ;; Yes WSGI Is still python
;;;  Djangoriffic
(defun django-time()
  (interactive)
  (add-to-list 'auto-mode-alist '("\\.html\\'" . django-mode)))
(require 'django-mode)

(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)



;;;;;;;;;;;;;; Custom set stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ac-dwim t)
 '(c-offsets-alist (quote ((brace-list-intro . 0) (label . 0))))
 '(mumamo-submode-indent-offset 2)
 '(newsticker-retrieval-method (quote extern))
 '(newsticker-url-list (quote (("A Softer World" "http://www.rsspect.com/rss/asw.xml" nil nil nil) ("A List Apart" "http://www.alistapart.com/feed/rss.xml" nil nil nil) ("BBC Sport" "http://newsrss.bbc.co.uk/rss/sportonline_uk_edition/football/rss.xml" nil nil nil) ("Dinosaur Comics" "http://www.rsspect.com/rss/qwantz.xml" nil nil nil) ("Dilbert" "http://feedproxy.google.com/DilbertDailyStrip" nil nil nil) ("Radar" "http://feeds.feedburner.com/oreilly/radar/atom" nil nil nil) ("Daily WTF" "http://syndication.thedailywtf.com/TheDailyWtf" nil nil nil) ("XKCD" "http://xkcd.com/atom.xml" nil nil nil) ("ORG" "http://www.openrightsgroup.org/feed/" nil nil nil) ("QC" "http://www.questionablecontent.net/QCRSS.xml" nil nil nil))))
 '(org-agenda-files nil)
 '(python-python-command "python")
 '(rails-ws:default-server-type "webrick"))


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
                           (erc :server "irc.freenode.net" :port "6667"
                                :nick "davidmiller")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;; Twitter ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/twit")  ; Save directory

;; Define M-x commands

(autoload 'twit-show-recent-tweets      "twit" "" t) ; most recent direct tweets (!)
(autoload 'twit-show-at-tweets          "twit" "" t) ; directed to you
(autoload 'twit-show-friends            "twit" "" t) ; your friends
(autoload 'twit-show-followers          "twit" "" t) ; your followers

(autoload 'twit-follow-recent-tweets    "twit" "" t) ; at idle, check at background

(autoload 'twit-post                    "twit" "" t)
(autoload 'twit-post-region             "twit" "" t)
(autoload 'twit-post-buffer             "twit" "" t)
(autoload 'twit-direct                  "twit" "" t) ; tweet to person

(autoload 'twit-add-favorite            "twit" "" t) ; Add to favourite: (*) star
(autoload 'twit-remove-favorite         "twit" "" t)

(autoload 'twit-add-friend              "twit" "" t) ; follow a friend
(autoload 'twit-remove-friend           "twit" "" t) ; emove a frienda

;; Customize twit-multi-accounts in order to use these: ((user . pass) ...)
(autoload 'twit-switch-account          "twit" "" t)
(autoload 'twit-direct-with-account     "twit" "" t)
(autoload 'twit-post-with-account       "twit" "" t)

(autoload 'twit-show-direct-tweets-with-account "twit" "" t)
(autoload 'twit-show-at-tweets-with-account     "twit" "" t)

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

(global-set-key "\C-cTpp" 'twit-post)                 ; (p)ost
(global-set-key "\C-cTpr" 'twit-post-region)          ; (p)post (r)egion
(global-set-key "\C-cTpb" 'twit-post-buffer)          ; (p)post (b)uffer
(global-set-key "\C-cTpr" 'twit-direct)               ; (p)post (d)irect
(global-set-key "\C-cTfa" 'twit-add-favorite)         ; (f)avorite (a)dd
(global-set-key "\C-cTfr" 'twit-remove-favorite)      ; (f)avorite (r)emove

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Web browsing stuff here innit ;;;;;;;;;;;;;;;;;;
(setq
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program "firefox")
(global-set-key "\C-cff" 'browse-url)
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
;(require 'icicles)
;(require 'icicles-iswitchb)
;(iswitchb-default-keybindings)
;icy-mode 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;  Keyboard Macros  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fset 'cp-line ;; Totally copying the current line
      "\C-e\C-a\C-k\C-y\C-e\C-j\C-y")

                                        ;(add-to-list 'load-path "~/.emacs.d/g-client")
                                        ;(load-library "g")


   ;;; ----------------------------------------------------------------
   ;;; caps-lock-mode, Miles Bader <miles /at/ gnu.org>

(defvar caps-lock-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'self-insert-upcased)
    map))

(define-minor-mode caps-lock-mode
  "When enabled, convert all self-inserting characters to uppercase."
  :lighter " CapsLock")

(defun self-insert-upcased (arg)
  (interactive "p")
  (setq last-command-char (upcase last-command-char))
  (self-insert-command arg))


;;;;;;;;;;;;;;;;;;;;;;;;;;;   ChessMate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-to-list 'load-path "~/.emacs.d/chess")
(autoload 'chess "chess" "Play a game of chess" t)


;;;;;;;;;;;;;;;;;;;;;;;;; GNUS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials       (expand-file-name "~/.authinfo")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "yourcompany.com")

(setq gnus-secondary-select-methods '((nntp "quimby.gnus.org")))
(setq gnus-subscribe-newsgroup-method 'gnus-subscribe-interactively)
(setq gnus-save-newsrc-file nil)
(setq gnus-always-read-dribble-file t)
(add-to-list 'load-path "~/.emacs.d/gnus-stuff")
(setq starttls-use-gnutls t
        starttls-gnutls-program "gnutls-cli"
        starttls-extra-arguments nil)

;;;;;;;;;;;;;;;;;;; Google ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/google.el")

(load "faith.el")

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad "
          "minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

;;;;;;;;;;;;;;;;;;; Elim ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(add-to-list 'load-path "~/elisp/elim")
;(autoload 'garak "garak" nil t)


;;;;;;;;;;;;;;; VCS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(require 'magit)
;(global-set-key "\C-cgs" 'magit-status)

;;;;;;;;;;;;;;;;; Saved States ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-c\C-w" 'jump-to-register)
(global-set-key [M-left] 'windmove-left) ; move to left windnow
(global-set-key [M-right] 'windmove-right) ; move to right window
(global-set-key [M-up] 'windmove-up) ; move to upper window
(global-set-key [M-down] 'windmove-down) ; move to downer window




(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(diff-file-header ((t (:background "grey60" :foreground "dark red" :weight bold))))
 '(diff-hunk-header ((t (:inherit diff-header :background "black" :foreground "firebrick"))))
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) (:background "#1f1f1f"))))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background dark)) (:background "#0f0f0f"))))
 '(twit-title-face ((((background light)) (:background "Black" :underline "whoite" :box (:line-width 2 :color "white" :style 0))) (((background dark)) (:background "Black" :underline "white" :box (:line-width 2 :color "white" :style 0))) (t (:underline "white")))))

;;  Yeah, IDO is actually better than Icicles - I think... although it fucks /sudo::
;;  if you tab at the wrong point
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

(add-to-list 'load-path "~/.emacs.d/emacs-rails/")
(require 'rails)


;; Via SteveYegge

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)


(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode 1))

;; Mysql
;(autoload 'mysql )
(add-to-list 'load-path "/home/david/.emacs.d/dvc/")
(require 'dvc-autoloads)
(global-set-key "\C-c\h\p" 'xhg-push)
(global-set-key "\C-c\g\p" 'xgit-push)
(setq dvc-tips-enabled nil)
(define-key ac-completing-map "\ESC/" 'ac-stop)