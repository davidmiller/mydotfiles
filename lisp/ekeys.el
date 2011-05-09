;; Keybindings

(global-set-key "\C-x\C-b" 'ibuffer) ;; Buffer management
;; Version Control
(global-set-key "\C-c\h\p" 'xhg-push)
(global-set-key "\C-c\g\p" 'xgit-push)
(global-set-key "\C-c\h\P" 'xhg-pull)
(global-set-key "\C-c\g\P" 'xgit-pull)
(global-set-key "\C-chl" 'xhg-log)
(global-set-key "\C-chd" 'xhg-log-toggle-diff-for-changeset)
(setq dvc-tips-enabled nil)

;; TODO - map xhg-log-toggle-diff-for-changeset inside
;; dvc-revlist. Probably a good idea to do xgit as well :)

(define-key global-map "\C-c\C-y" 'clipboard-yank); clipboard paste
(define-key global-map "\C-ccx" 'clipboard-kill-region); clipboard paste
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key "\C-c\C-w" 'jump-to-register)
(global-set-key [M-left] 'windmove-left) ; move to left windnow
(global-set-key [M-right] 'windmove-right) ; move to right window
(global-set-key [M-up] 'windmove-up) ; move to upper window
(global-set-key [M-down] 'windmove-down) ; move to downer window
(global-set-key "\M-#" 'find-tag-other-window)
(global-set-key "\C-cfp" 'flyspell-prog-mode)

;; Python
(global-set-key "\M-p" 'pyflakes-show-help)

;; Completions - let's get one set of keybindings regardless of context
(global-set-key (kbd "<backtab>") 'auto-complete)
(global-set-key [\C-\;] 'yas/next-field)
(global-set-key [\C\'] 'yas/expand)
