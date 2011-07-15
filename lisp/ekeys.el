;; Keybindings

(defmacro gset-key (pairs)
  "Globally set `pairs' as (binding symbol)"
  (let ((bindings (mapcar (lambda (b) (cons 'global-set-key b)) pairs)))
  `(progn ,@bindings)
  ))

(gset-key (
       ;; Buffer management
       ("\C-x\C-b" 'ibuffer)
       ("\C-c\#" 'ido-find-file-other-window)
       ((kbd "<f5>") #'(lambda () (revert-buffer t t)))
       ("\C-c\C-w" 'jump-to-register)
       ([M-left] 'windmove-left) ; move to left windnow
       ([M-right] 'windmove-right) ; move to right window
       ([M-up] 'windmove-up) ; move to upper window
       ([M-down] 'windmove-down) ; move to downer window
       ((kbd "S-C-<left>") 'shrink-window-horizontally)
       ((kbd "S-C-<right>") 'enlarge-window-horizontally)
       ((kbd "S-C-<down>") 'shrink-window)
       ((kbd "S-C-<up>") 'enlarge-window)

       ;; Version Control
       ("\C-c\h\p" 'xhg-push)
       ("\C-c\g\p" 'xgit-push)
       ("\C-c\h\P" 'xhg-pull)
       ("\C-c\g\P" 'xgit-pull)
       ("\C-chl" 'xhg-log)
       ("\C-chd" 'xhg-log-toggle-diff-for-changeset)

       ;; Python
       ("\M-p" 'pyflakes-show-help)

       ;; Show-hide
       ((kbd "<backtab>") 'hs-hide-level)
       ((kbd "<S-right>") 'hs-show-block)
       ((kbd "<S-down>") 'hs-show-all)
       ((kbd "<S-left>") 'hs-hide-block)
       ((kbd "<S-up>") 'hs-hide-all)

       ;; Editing

       ("\C-w" 'backward-kill-word)
       ("\C-x\C-k" 'kill-region)
       ("\C-c\C-k" 'kill-region)

       ;; Save excursion inserts
       ((kbd "C-M-;") 'colonize)
       ((kbd "C-M-,") 'commatize)

       ("\M-#" 'find-tag-other-window)
       ("\C-cfp" 'flyspell-prog-mode)

       ))

(define-key global-map "\C-c\C-y" 'clipboard-yank); clipboard paste
(define-key global-map "\C-ccx" 'clipboard-kill-region); clipboard paste

(setq dvc-tips-enabled nil)







;; Completions - let's get one set of keybindings regardless of context
(global-set-key [\C-\;] 'yas/next-field)
(global-set-key [\C\'] 'yas/expand)


(global-set-key "\C-cR" 'rename-current-file-or-buffer)

;; Lisp
(global-set-key "\C-c\m" 'pp-macroexpand-last-sexp)

(global-set-key (kbd "C-M-S-j") #'(lambda () (interactive) (previous-line) (move-end-of-line nil) (newline-and-indent)))
(global-set-key (kbd "C-M-j") #'(lambda () (interactive) (move-end-of-line nil) (newline-and-indent)))

(provide 'ekeys)
