;; Languages

;;;;  Python
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(add-to-list 'auto-mode-alist '("\\.wsgi\\'" . python-mode))  ;; Yes WSGI Is still python

(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
(load-library "django")

;; Javascript
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)

;;;;  Lisp

                                        ;SLIME interaction
(setq inferior-lisp-program "clisp")
(require 'slime)
(slime-setup)
(global-font-lock-mode t)
(show-paren-mode 1)
(add-hook 'lisp-mode-hook '(lambda ()
                             (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'emacs-lisp-mode '(lambda () (pretty-lambdas)))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))


;;;; Ruby
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
(require 'rails)

;;;;  PHP
(load-library "php-mode")
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . html-mode))
