; Set global Whoami Vars
(setq user-mail-address "david@deadpansincerity.com")
(setq user-full-name "David Miller")

;; Let's get mail from Google
(setq gnus-select-method '(nnimap "gmail"
(nnimap-address "imap.gmail.com")
(nnimap-server-port 993)
(nnimap-stream ssl)))


(add-to-list `load-path "~/.emacs.d/gnus-stuff")
(require 'randomsig)
(define-key message-mode-map (kbd "C-c s") 'randomsig-replace-sig)
(define-key message-mode-map (kbd "C-c S") 'randomsig-select-sig)
(require 'gnus-sum) ; probably required for `gnus-summary-save-map'
(define-key gnus-summary-save-map "-" 'gnus/randomsig-summary-read-sig)
(setq randomsig-dir "~/")
(setq randomsig-files '(".sig"))
;; or (setq randomsig-files (randomsig-search-sigfiles))
;; or (setq randomsig-files 'randomsig-search-sigfiles)
(setq message-signature 'randomsig-signature)
(setq randomsig-delimiter-pattern "^--")
(setq randomsig-static-string "Love regards etc\n\nDavid Miller\nwww.deadpansincerity.com\n07964250347\n--\nThis message sent from Emacs\n")
(setq gnus-thread-sort-functions 'gnus-thread-sort-by-most-recent-date)

(add-to-list 'load-path "~/.emacs.d/bbdb")
(require 'bbdb)
(bbdb-initialize 'gnus 'message)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus) 

;;;;;;;;;;;  Not exactly sure what these are doing... ;;;;;;;;;;;;;;;


(cond (window-system
       (setq custom-background-mode 'light)
       (defface my-group-face-1
         '((t (:foreground "Red" :bold t))) "First group face")
       (defface my-group-face-2
         '((t (:foreground "DarkSeaGreen4" :bold t)))
         "Second group face")
       (defface my-group-face-3
         '((t (:foreground "Green4" :bold t))) "Third group face")
       (defface my-group-face-4
         '((t (:foreground "SteelBlue" :bold t))) "Fourth group face")
       (defface my-group-face-5
         '((t (:foreground "Blue" :bold t))) "Fifth group face")))

(setq gnus-group-highlight
      '(((> unread 200) . my-group-face-1)
        ((and (< level 3) (zerop unread)) . my-group-face-2)
        ((< level 3) . my-group-face-3)
        ((zerop unread) . my-group-face-4)
        (t . my-group-face-5)))
