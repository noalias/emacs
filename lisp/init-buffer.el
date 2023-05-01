;;; -*- lexical-binding: t -*-
(use-package emacs
  :demand
  :bind
  (("C-x C-k" . kill-buffer-and-window)
   ("C-x x p" . switch-to-prev-buffer)
   ("C-x x n" . switch-to-next-buffer)
   ("C-x x o" . consult-buffer-other-window)
   :map global:commands-map
   ("i" . ibuffer)
   ("s" . save-buffer)
   ("k" . kill-current-buffer)
   ("p" . switch-to-prev-buffer)
   ("n" . switch-to-next-buffer))
  :init
  (defvar buffer:skip-regexp
    (rx bos
        ?*
        (or "Messages"
            "Output"
            "Compile-Log"
            "Completions"
            "Warnings"
            "Flymake diagnostics"
            "Async Shell Command"
            "Async-native-compile-log"
            "Native-compile-Log"
            "Apropos"
            "Backtrace"
            "prodigy"
            "help"
            "Calendar"
            "lsp-bridge"
            "Embark Actions"
            "Finder"
            "Kill Ring"
            "Embark Export:"
            "eshell"
            "epc con"
            "shell"
            "terminal"
            "vterm"
            "quickrun"
            "elfeed-entry"
            "macro expansion"
            "Agenda Commands"
            "Org Select"
            "Capture"
            "CAPTURE-"
            "prolog"
            "rustfmt"
            "Disabled Command"
            "straight-byte-compilation"
            "straight-process"
            )
        (* anything)
        ?*
        eos))
  :config
  (setq switch-to-prev-buffer-skip-regexp buffer:skip-regexp)
  
  (use-package consult
    :defer t
    :config
    (add-to-list 'consult-buffer-filter buffer:skip-regexp))
  )

(provide 'init-buffer)
;;; init-buffer.el ends here
