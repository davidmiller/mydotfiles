(require 'cl)

;; I keep all my emacs-related stuff under ~/emacs
(defvar emacs-root "/home/david/")

;; add all the elisp directories under ~/emacs to my load path
(labels ((add-path (p)
     (add-to-list 'load-path
            (concat emacs-root p))))
 (add-path "emacs/lisp")
 (add-path "emacs/auto-complete")
 (add-path "emacs/dvc")
 (add-path "emacs/django")
 (add-path "emacs/emacs-rails")
 (add-path "emacs/gnus-stuff")
 (add-path "emacs/major-modes")
 (add-path "emacs/minor-modes")
 (add-path "emacs/slime")
 (add-path "emacs/themes")
 (add-path "emacs/yasnippet")
 )
(load-library "econf")
(load-library "ekeys")
(load-library "ecolours")
(load-library "efuncs")
(load-library "elangs")

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Jabber client ;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/emacs/emacs-jabber-0.8.0")
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;; Web browsing stuff here innit ;;;;;;;;;;;;;;;;;;
(setq
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program "firefox")
(global-set-key "\C-cff" 'browse-url)
;;(setq browse-url-browser-function "firefox")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ORG mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(add-to-list 'load-path "~/emacs/gnus-stuff")
(setq starttls-use-gnutls t
        starttls-gnutls-program "gnutls-cli"
        starttls-extra-arguments nil)


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(rst-level-1-face ((t (:background "black" :foreground "brown3" :weight bold))) t)
 '(rst-level-2-face ((t (:background "black" :foreground "white" :weight bold))) t))
