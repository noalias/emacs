;;;  -*- lexical-binding: t -*-
;;; `treesit'
(use-package treesit
  :when (treesit-available-p)
  :init (setq major-mode-remap-alist
              '((cmake-mode      . cmake-ts-mode)
                (conf-toml-mode  . toml-ts-mode)
                (css-mode        . css-ts-mode)
                (go-mode         . go-ts-mode)
                (json-mode       . json-ts-mode)
                (js-json-mode    . json-ts-mode)
                (js-mode         . js-ts-mode)
                (python-mode     . python-ts-mode)
                (sh-mode         . bash-ts-mode)
                (typescript-mode . typescript-ts-mode))))

(use-package treesit-auto
  :if base:linux-p
  :straight t
  :custom
  (treesit-auto-install t)
  :config
  (global-treesit-auto-mode))

;;; `prog-mode'
(use-package prog-mode
  :defer
  :config
  (add-hook 'prog-mode-hook #'global-prettify-symbols-mode)
  (add-hook 'prog-mode-hook #'eldoc-mode)
  
  (defun prog:indent-spaces-mode ()
    (setq indent-tabs-mode nil)))

;;;; `elisp-mode'
(use-package elisp-mode
  :defer
  :config
  (add-hook 'emacs-lisp-mode-hook #'reveal-mode)
  (add-hook 'lisp-interaction-mode-hook #'prog:indent-spaces-mode))

;;;; `rust-mode'
(use-package rust-ts-mode
  :when (treesit-ready-p 'rust)
  :mode "\\.rs\\'")

;;; `eglot'
(use-package eglot
  :hook (rust-ts-mode-hook . eglot-ensure))

;;;; `other'
(provide 'init-prog)
