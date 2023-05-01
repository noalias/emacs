;;; -*- lexical-binding: t -*-
(use-package rime
  :straight t
  :after init-face
  :init
  (setq default-input-method "rime")
  :bind
  (:map rime-active-mode-map
        ("TAB" . rime-inline-ascii))
  :custom
  (rime-user-data-dir "~/.config/rime/emacs")
  (rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g" "C-v" "M-v" "," "." "S-<return>"))
  (rime-inline-ascii-holder ?a)
  (rime-cursor "|")
  ;; 与`rime-cursor'互斥 
  (rime-show-preedit 'inline)
  ;; rime-show-candidate 'minibuffer
  (rime-title "rime")
  (rime-inline-ascii-trigger 'shift-r)
  (rime-show-candidate 'posframe) ;; or `minibuffer'
  (rime-posframe-properties (list :background-color "#100f12"
                                  :foreground-color "#dd4118"
                                  :font face:fontset-font
                                  :internal-border-width 7))
  (rime-disable-predicates '(rime-predicate-prog-in-code-p))
  (rime-inline-predicates '(rime-predicate-space-after-cc-p
                            rime-predicate-after-ascii-char-p
                            rime-predicate-tex-math-or-command-p)))

(provide 'init-input-method)
;;; init-input-method.el ends here
