;; Set the default font here
(set-default-font
 "-unknown-DejaVu Sans Mono-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")


;; Colours

(require 'color-theme)
(defun color-theme-bluebulator ()
  (interactive)
  (color-theme-install
   '(color-theme-bluebulator
      ((background-color . "#0f0f0f")
      (background-mode . light)
      (border-color . "#191919")
      (cursor-color . "#43667f")
      (foreground-color . "#c9c9c5")
      (mouse-color . "black"))
     (fringe ((t (:background "#191919"))))
     (mode-line ((t (:foreground "#a3a3a3" :background "#163e60"))))
     (region ((t (:background "#29383d"))))
     (font-lock-builtin-face ((t (:foreground "#ccaa61"))))
     (font-lock-comment-face ((t (:foreground "brown"))))
     (font-lock-function-name-face ((t (:foreground "#197db8"))))
     (font-lock-keyword-face ((t (:foreground "#508195"))))
     (font-lock-string-face ((t (:foreground "#6ea07f"))))
     (font-lock-type-face ((t (:foreground"#539355"))))
     (font-lock-variable-name-face ((t (:foreground "#7f989f"))))
     (minibuffer-prompt ((t (:foreground "#a2c4d8" :bold t))))
     (font-lock-warning-face ((t (:foreground "Red" :bold t))))
     )))
(provide 'color-theme-bluebulator)
(color-theme-bluebulator)

(defun pretty-lambdas ()
    (font-lock-add-keywords
     nil `(("(\\(lambda\\>\\)"
            (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                      ,(make-char 'greek-iso8859-7 107))
                      nil))))))

(custom-set-faces
 '(rst-level-1-face ((t (:background "black" :foreground "brown3" :weight bold))) t)
 '(rst-level-2-face ((t (:background "black" :foreground "white" :weight bold))) t))
