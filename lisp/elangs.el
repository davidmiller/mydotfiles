
;; Languages

;; CSS
(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

;;;;  Python

;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")
(add-to-list 'auto-mode-alist '("\\.wsgi\\'" . python-mode))
(setq ropemacs-enable-autoimport t)
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
(load-library "pony")

(require 'flymake)
;; Pyflakes for highlighting syntax errors
(defun flymake-pyflakes-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "pyflakes" (list local-file))))

(defun pyflakes-show-help ()
  (interactive)
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))



(add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-pyflakes-init))

(defun rope-eldoc-function ()
  (interactive)
  (let* ((win-conf (current-window-configuration))
         (resize-mini-windows nil)
         (disable-python-trace t)
         class fun args result-type
         (flymake-message (python-flymake-show-help))
         (initial-point (point))
         (paren-range (let (tmp)
                        (ignore-errors
                          (setq tmp (vimpulse-paren-range 0 ?\( nil t))
                          (if (and tmp (>= (point) (car tmp)) (<= (point) (cadr tmp)))
                              tmp
                            nil))))
         (result (save-excursion
                   ;; check if we on the border of args list - lparen or rparen
                   (if paren-range
                       (goto-char (car paren-range)))
                   (call-interactively 'rope-show-doc)
                   (set-buffer "*rope-pydoc*")
                   (goto-char (point-min))
                   (if (or (equal (point-max) 1)
                           (not (re-search-forward "\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)(.*):" (point-at-eol) t))
                           (and (current-message) (string-match-p "BadIdentifierError" (current-message))))
                       nil
                     (let (result)
                       ;; check if this is class definition
                       (if (looking-at "class \\([a-zA-Z_]+[a-zA-Z0-9_]*\\)(.*):")
                           (progn
                             (goto-char (point-at-eol))
                             (re-search-forward (buffer-substring (match-beginning 1) (match-end 1)))))
                       (goto-char (point-at-bol))
                       (setq result (buffer-substring (point) (point-at-eol)))

                       ;; check if exist better description of function
                       (goto-char (point-at-eol))
                       (string-match "\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)(.*)" result) ;get function name
                       (if (re-search-forward (concat (match-string 1 result) "(.*)") nil t)
                           (progn
                             (goto-char (point-at-bol))
                             (setq result (buffer-substring (point) (point-at-eol)))))

                       ;; return result
                       result
                       ))))
         (arg-position (save-excursion
                         (if paren-range
                             (count-matches "," (car paren-range) (point))))))
    ;; save window configuration
    (set-window-configuration win-conf)
    ;; process main result
    (if result
        (progn
          (setq result-type (nth 1 (split-string result "->")))
          (setq result (nth 0 (split-string result "->")))
          (setq result (split-string result "("))
          (setq fun (nth 1 (split-string (nth 0 result) "\\.")))
          (setq class (nth 0 (split-string (nth 0 result) "\\.")))
          ;; process args - highlight current function argument
          (setq args (nth 0 (split-string (nth 1 result) ")")))

          ;; highlight current argument
          (if args
              (progn
                (setq args (split-string args ","))
                (setq args (let ((num -1))
                             (mapconcat
                              (lambda(x)(progn
                                          (setq num (+ 1 num))
                                          (if (equal num arg-position) (propertize x 'face 'eldoc-highlight-function-argument) x)))
                              args
                              ",")))))

          ;; create string for type signature
          (setq result
                (concat
                 (propertize "Signature: " 'face 'flymake-message-face)

                 (if fun
                     (concat (propertize (org-trim class) 'face 'font-lock-type-face)
                             "."
                             (propertize (org-trim fun) 'face 'font-lock-function-name-face))
                   (propertize (org-trim class) 'face 'font-lock-function-name-face))

                 " (" args ")"

                 (if result-type
                     (concat " -> " (org-trim result-type)))
                 ))))

    ;; create final result
    (if (and (null flymake-message) (null result))
        nil
      (concat flymake-message
              (if (and result flymake-message) "\n")
              result))))

(defvar disable-python-trace nil)

(defadvice message(around message-disable-python-trace activate)
  (if disable-python-trace
      t
    ad-do-it))

(defface flymake-message-face
  '((((class color) (background light)) (:foreground "#b2dfff"))
    (((class color) (background dark))  (:foreground "#b2dfff")))
  "Flymake message face")

(defun python-flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help
          (format (concat (propertize "Error: " 'face 'flymake-message-face) "%s") help)))))



(add-hook 'python-mode-hook
          '(lambda ()
             ;; Flymake loads itself in the temp buffers used by
             ;; `py-execute-region` because it calls (python-mode) in
             ;; the temp buffer. Let's stop this, because
             ;; a) it's silly
             ;; b) It prevents execution.
             ;; [So we check for not being in a temp buffer]
             (unless (eq buffer-file-name nil)
               (progn
                 (flymake-mode t)
                 (set (make-local-variable
                       'eldoc-documentation-function)
                      'rope-eldoc-function)
;                 (turn-on-eldoc-mode)
                 (light-symbol-mode t)))))


;; Nose integration with python-mode
(require 'test-case-mode)
(setq-default
 mode-line-format
 (mapcan (lambda (x)
           (unless (and (consp x)
                    (stringp (car x))
                    (eq 'test-case-dot-tooltip
                        (get-text-property 0 'help-echo (car x))))
             (list x)))
         (default-value 'mode-line-format)))

(global-set-key (kbd "<f9>") 'test-case-run-without-pdb)
(global-set-key (kbd "S-<f9>") 'test-case-run-with-pdb)

(defun test-case-run-without-pdb ()
  (interactive)
  (unless test-case-mode (test-case-mode 1))
  (set (make-local-variable 'test-case-nose-arguments) "-d")
  (test-case-run))

(defun test-case-run-with-pdb ()
  (interactive)
  (unless test-case-mode (test-case-mode 1))
  (set (make-local-variable 'test-case-nose-arguments) "--pdb --pdb-failures")
  (test-case-run))

(eval-after-load 'python-mode
  '(add-hook 'python-mode-hook 'enable-test-case-mode-if-test))

(defcustom test-case-nose-executable "nosetests"
  "*The nosetests executable."
  :group 'test-case :type 'file)
(defcustom test-case-nose-arguments "-d --with-coverage"
  "*The nosetests arguments."
  :group 'test-case :type 'string)
(defcustom test-cwd "../"
  "*The directory from which to run nosetests. Should be set per-buffer."
  :group 'test-case :type 'file :safe 'stringp)
(defcustom test nil
  "*The test file to run instead of this file."
  :group 'test-case :type 'file :safe 'stringp)

(defvar test-case-nose-font-lock-keywords
  (eval-when-compile
    `((,(concat "\\_<\\(?:assert\\|raises\\)\\_>")
       (0 'test-case-assertion append)))))

(defun test-case-nose-failure-pattern ()
  (let ((file (regexp-quote (or test buffer-file-name))))
    (list (concat "  File \"\\(\\(" file "\\)\", line \\([0-9]+\\)\\).*\n"
                  "\\(?:  .*\n\\)*"
                  "\\([^ ].*\\)"
                  )
          2 3 nil nil 4)))

(defun test-case-nose-process-filter (proc string)
  "Filter to switch to comint-mode once Pdb is activated by nose."
  (let ((proc-buffer (process-buffer proc))
        (inhibit-read-only t))
    (with-current-buffer proc-buffer
      (insert string)
      (when (string-match "(Pdb.*) $" string)
        (toggle-read-only 0)
        (comint-mode)
        (set-process-filter proc 'comint-output-filter)
        (goto-char (point-max))
        (set-marker (process-mark proc) (point))
        ;; enable pdbtrack
        (when (fboundp 'py-pdbtrack-track-stack-file)
          (add-hook 'comint-output-filter-functions 'py-pdbtrack-track-stack-file)
          (setq py-pdbtrack-do-tracking-p t))
        ;; show a backtrace
        (insert "bt")
        (ignore-errors (comint-send-input))
        ;; and switch to Pdb buffer
        (pop-to-buffer proc-buffer)))))

(defun test-case-nose-backend (command)
  "Python nose back-end for `test-case-mode'."
  (case command
    ('name "Nose")
    ('supported (derived-mode-p 'python-mode))
    ('command (concat "cd " test-cwd "; " test-case-nose-executable " "
                      test-case-nose-arguments " " (or test buffer-file-name)))
    ('run-hook
     (set-process-filter (get-buffer-process (current-buffer))
                         'test-case-nose-process-filter))
    ('save t)
    ('failure-pattern (test-case-nose-failure-pattern))
    ('font-lock-keywords test-case-nose-font-lock-keywords)))

(add-to-list 'test-case-backends 'test-case-nose-backend t)

;; Javascript
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)

;;;;  Lisp

                                        ;SLIME interaction
(setq inferior-lisp-program "/usr/bin/sbcl")
(require 'slime)
(slime-setup '(slime-fancy))
(global-font-lock-mode t)
(show-paren-mode 1)
(add-hook 'lisp-mode-hook '(lambda ()
                             (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'emacs-lisp-mode '(lambda ()
                              (pretty-lambdas)
                              (autopair-mode t)
                              (light-symbol-mode t)))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))


;;;; Ruby
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
(require 'rails)

;;;;  PHP
(load-library "php-mode")
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . html-mode))

;; Erlang
(load-library "erlang")
(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))
(defun my-erlang-hook ()
  "TEsting purps only"
  (message "called")
  (auto-complete-mode t))
(add-hook 'erlang-mode-hook 'my-erlang-hook)
