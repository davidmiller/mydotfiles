;;
;; emodes.el
;;
;; Commentary:
;;
;; These are similar to c-styles (see c-style-alist) in intention, only
;; rather than identifying indentation styles, they identify feature styles
;; that one might like to group together e.g. Text-style, IDE-style and then
;; invoke in particular language hooks.
;;

;; Code:
(require 'smart-operator)

;(defmacro )

(setq ide-style
  '(lambda ()
     (make-ret-indenting)
     (pretty-lambdas)
     (show-paren-mode 1)
     (autopair-mode t)
     (imenu-add-menubar-index)
     (hs-minor-mode t)
     (which-func-mode t)
     (smart-operator-mode-on)
     (light-symbol-mode t)))

(defvar colourful-style
  '(lambda ()
     (rainbow-mode t))
  "A mode-style that assumes we'll be dealing with colourful material")

(defvar textual-style
  '(lambda ()
     (longlines-mode t)
     (flyspell-mode t)))

(defun set-mode-style (style)
  "Set the mode-style to `style`"
  (set (make-local-variable 'mode-style) (symbol-name 'style))
  (funcall style))

(provide 'emodes)