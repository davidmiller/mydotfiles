;; Custom

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


;; move (shift) a line of text up or down like you would do in Eclipse
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (let ((col (current-column))
        start
        end)
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (forward-char)
    (setq end (point))
    (let ((line-text (delete-and-extract-region start end)))
      (forward-line n)
      (insert line-text)
      ;; restore point to original column in moved line
      (forward-line -1)
      (forward-char col))))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(global-set-key (kbd "<M-kp-8>") 'move-line-up)
(global-set-key (kbd "<M-kp-2>") 'move-line-down)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;  Keyboard Macros  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fset 'cp-line ;; Totally copying the current line
      "\C-e\C-a\C-k\C-y\C-e\C-j\C-y")
