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

(defvar ide-style
  (lambda ()
    (make-ret-indenting)
    (pretty-lambdas)
    (show-paren-mode 1)
    (autopair-mode t)
    (imenu-add-menubar-index)
    (hs-minor-mode t)
    (which-func-mode t)
    (smart-operator-mode t)
    (light-symbol-mode t))
  "A programming-mode style that will enable groups of IDE-like features.")

(defvar colourful-style
  (lambda ()
    (rainbow-mode t))
  "A mode-style that assumes we'll be dealing with colourful material")

(defvar textual-style
  (lambda ()
    (longlines-mode t)
    (flyspell-mode t)))

(defun set-mode-style (style)
  "Set the mode-style to `style`"
  (funcall style))