;; David's .emacs
(require 'cl)
(defvar emacs-root "/home/david/")
(labels ((add-path (p)
                   (add-to-list 'load-path
                                (concat emacs-root p))))

(add-path "emacs/lisp")
(add-path "emacs/auto-complete")
(add-path "emacs/yasnippet")
;(add-path "emacs/themes")
(add-path "emacs/major-modes")
(add-path "emacs/django")
(add-path "emacs/slime")
(add-path "emacs/emacs-rails")
)
(load-library "econf")
(load-library "efuncs")
(load-library "ekeys")
(load-library "ecolours")
(load-library "elangs")
(load-library "django")


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Jabber client ;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/emacs/emacs-jabber-0.8.0")
(load "jabber-autoloads")
(setq jabber-account-list
      '(("david@deadpansincerity.com"
         (:network-server . "talk.google.com")
         (:connection-type . ssl))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; erc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connect to Freenode on C-c e f
(global-set-key "\C-cef" (lambda () (interactive)
                           (erc :server "irc.freenode.net" :port "6667"
                                :nick "davidmiller")))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;; Web browsing stuff here innit ;;;;;;;;;;;;;;;;;;
(setq
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program "firefox")
(global-set-key "\C-cff" 'browse-url)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ORG mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;; ;;;;;;;;;;;;;;;;;;;;;;;;; GNUS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; IDO
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching


;; ;; Version Control
(add-to-list 'load-path "~/emacs/dvc/")
(require 'dvc-autoloads)
(setq dvc-tips-enabled nil)
(define-key ac-completing-map "\ESC/" 'ac-stop)

;; Remote sudo with Tramp
;; (add-to-list 'tramp-default-proxies-alist
;;              '(nil "\\`root\\'" "/ssh:%h:"))
;; (add-to-list 'tramp-default-proxies-alist
;;              '((regexp-quote (system-name)) nil nil))


(add-hook 'Info-mode-hook           ; After Info-mode has started
          (lambda ()
            (setq Info-additional-directory-list Info-default-directory-list)
            ))