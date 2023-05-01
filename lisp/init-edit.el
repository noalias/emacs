;;;  -*- lexical-binding: t -*-
(use-package emacs
  :init
  :config
  ;; 设置光标形状
  (setq-default cursor-type 'bar)
  (add-hook 'after-init-hook #'global-hl-line-mode)
  (add-hook 'after-init-hook #'electric-pair-mode)
  (add-hook 'after-init-hook #'show-paren-mode)
  (add-hook 'after-init-hook #'auto-insert-mode)
  (setq auto-insert-directory (expand-file-name "template" user-emacs-directory))
  (setq show-paren-sytle 'parentthesis)
  (setq-default fill-column 80
                tab-width 4
                indent-tabs-mode nil))

(use-package outline
  :hook (prog-mode-hook . outline-minor-mode)
  :bind
  (:map outline-mode-prefix-map
        ("b" . outline-backward-same-level)
        ("f" . outline-forward-same-level)
        ("p" . outline-previous-visible-heading)
        ("n" . outline-next-visible-heading)
        ("u" . outline-up-heading)
        ("]" . outline-move-subtree-down)
        ("[" . outline-move-subtree-up)
        (">" . outline-demote)
        ("<" . outline-promote)
        ("i" . outline-show-children)
        ("l" . outline-show-subtree)
        ("h" . outline-hide-subtree)
        ("t" . outline-hide-body)
        ("a" . outline-show-all)
        ("c" . outline-hide-entry)
        ("e" . outline-show-entry)
        ("k" . outline-hide-leaves)
        ("v" . outline-show-branches)
        ("q" . outline-hide-sublevels)
        ("o" . outline-hide-other)
        ("m" . outline-insert-heading))
  :custom
  (outline-minor-mode-prefix (kbd "M-o"))
  :config
  (set-display-table-slot standard-display-table 
                          'selective-display 
                          (string-to-vector " ⤵")))

(use-package avy
  :straight t
  :bind
  (("C-'" . avy-goto-char)
   ("M-'" . avy-goto-char-2)
   ("C-," . avy-goto-word-crt-line)
   ("M-g w" . avy-goto-word-1)
   ("M-g e" . avy-goto-word-0)
   ("M-g g" . avy-goto-line))
  :config
  (setq avy-background t)
  (defun avy-goto-word-crt-line ()
    "Jump to a word start on the current line only."
    (interactive)
    (avy-with avy-goto-word-0
              (avy-goto-word-0 nil (line-beginning-position) (line-end-position)))))

(use-package aggressive-indent
  :straight t
  :hook (after-init-hook . global-aggressive-indent-mode))

(use-package symbol-overlay
  :straight t
  :bind
  (("M-i" . symbol-overlay-put)
   ("M-n" . symbol-overlay-switch-forward)
   ("M-p" . symbol-overlay-switch-backward)))

(provide 'init-edit)
