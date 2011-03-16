;; Keybindings
;;
;;

(global-set-key "\C-x\C-b" 'ibuffer) ;; Buffer management
(global-set-key "\C-c\h\p" 'xhg-push)
(global-set-key "\C-c\g\p" 'xgit-push)
(global-set-key "\C-c\h\P" 'xhg-pull)
(global-set-key "\C-c\g\P" 'xgit-pull)
(setq dvc-tips-enabled nil)
(define-key global-map "\C-c\C-y" 'clipboard-yank); clipboard paste
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key "\C-c\C-w" 'jump-to-register)
(global-set-key [M-left] 'windmove-left) ; move to left windnow
(global-set-key [M-right] 'windmove-right) ; move to right window
(global-set-key [M-up] 'windmove-up) ; move to upper window
(global-set-key [M-down] 'windmove-down) ; move to downer window
(global-set-key "\M-#" 'find-tag-other-window)