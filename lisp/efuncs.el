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
