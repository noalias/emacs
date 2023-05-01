;;;  -*- lexical-binding: t -*-
(use-package minibuffer
  :bind
  (:map minibuffer-local-completion-map
        ("M-j" . minibuffer-next-completion)
        ("M-k" . minibuffer-previous-completion)
        ("C-RET" . completion:force-exit)
        ("SPC")
        :map completion-list-mode-map
	    ("z" . switch-to-minibuffer))
  :custom
  ;; (minibuffer-electric-default-mode t)
  ;; Don't insert completion at point into minibuffer
  ;; `M-<RET>' complete `minibuffer'
  (minibuffer-completion-auto-choose nil)
  ;; One frame one minibuffer.
  (minibuffer-follows-selected-frame nil)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (minibuffer-default-prompt-format " [%s]")
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  (resize-mini-windows t)
  (completion-auto-help t)
  (completion-show-help nil)
                                        ;(completion-show-inline-help nil)
  (completion-cycle-threshold nil)
  ;; `t' `second-tab' `nil'
  (completion-auto-select 'seond-tab)
  (completions-detailed t)
  ;; Ignore cases when complete
  (completion-ignore-case t)
  ;; vertical display
  (completions-format 'one-column)
  (completions-max-height 13)
  (completions-sort #'completion:list-sort)
  :config


  (defun completion:list-sort (all)
    "对 `Completions-buffer' 中的补全项进行排序"
    (let ((hist (minibuffer-history-value)))
      (thread-first all
                    (sort (lambda (c1 c2) (< (length c1) (length c2))))
                    (sort (lambda (c1 c2) (> (length (member c1 hist))
                                         (length (member c2 hist))))))))
  ;; Copy from `icomplete'
  (defun completion:force-exit (force)
    "Attempt to exit minibuffer immediately with current input.
Unless FORCE is non-nil (interactively with a prefix argument),
honor a non-nil REQUIRE-MATCH argument to `completing-read' by
trying to complete as much as possible and disallowing the exit
if that doesn't produce a completion match."
    (interactive "P")
    (if (and (not force) minibuffer--require-match)
        (minibuffer-complete-and-exit)
      (exit-minibuffer)))
  )

(use-package aggressive-completion
  :straight t
  :bind
  (:map aggressive-completion-minibuffer-map
        ("TAB" . completion:auto-select))
  :hook after-init-hook
  :config
  (add-hook 'aggressive-completion-mode-hook #'completion:disable-auto-select)
  (defun completion:disable-auto-select ()
    (setq
     ;; 避免自动切换至 `Completions-buffer' 中的补全项
     completion-auto-select nil
     ;; 禁止 `minibuffer' 中的补全     
     completion-cycle-threshold nil
     ;; 禁止自动插入 `Completions-buffer' 中的补全
     minibuffer-completion-auto-choose nil))
  
  (defun completion:auto-select ()
    "`TAB' 键可切换至 `Completions-buffer' 中的补全项"
    (interactive)
    (let ((completion-auto-select t))
      (minibuffer-complete))))

(use-package company
  :straight t
  :hook
  (after-init-hook . global-company-mode)
  :custom
  (company-backends '((company-capf :with company-yasnippet)
                      (company-keywords company-files)))
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 12)
  (company-tooltip-offset-display 'line)
  :config
  (setq company-global-modes '(not message-mode
                                   help-mode
                                   eshell-mode
                                   shell-mode))
  (setq company-idle-delay 0
        company-minimum-prefix-length 3
        company-icon-margin 3
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil))

(use-package consult-company
  :straight t
  :after (company consult)
  :bind
  (:map company-active-map
        ("C-s" . consult-company))
  :config
  (consult-customize
   consult-company
   :initial company-prefix
   (defun completion:consult-company-frontend (command)
     (pcase command
       (`post-command (consult-company)))))
  )

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package embark
  :straight t
  :bind
  (("C-;" . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings)
   :map minibuffer-local-completion-map
   ("M-o" . embark-export))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :custom
  (embark-help-key "?")
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Embark " (* anychar) eos)
                 nil
                 (window-parameters (mode-line-format . none))))
                                        ;(setq embark-prompter 'embark-completing-read-prompter)
  )

(use-package embark-consult
  :straight t
  :after (consult embark)
  :config
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(use-package consult
  :straight t
  :demand t
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :bind
  (([remap switch-to-buffer] . consult-buffer)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap bookmark-jump] . consult-bookmark)
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ([remap yank-pop] . consult-yank-pop)
   ([remap goto-line] . consult-goto-line)
   ([remap imenu] . consult-imenu)
   ([remap isearch-forward] . consult-line))
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-async-input-debounce 0.1
        consult-async-input-throttle 0.2
        consult-narrow-key "<"
        consult-line-number-widwn t)
  
  (defvar consult--fd-command nil)
  (if base:win-p
      (modify-coding-system-alist 'process "fd" '(utf-8 . gbk)))
  
  (defun consult--fd-builder (input)
    (unless consult--fd-command
      (setq consult--fd-command
            (if (eq 0 (call-process-shell-command "fdfind"))
                "fdfind"
              "fd")))
    (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                 (`(,re . ,hl) (funcall consult--regexp-compiler
                                        arg 'extended t)))
      (when re
        (list :command (append
                        (list consult--fd-command
                              "--color=never" "--full-path"
                              (consult--join-regexps re 'extended))
                        opts)
              :highlight hl))))
  
  (defun consult-fd (&optional dir initial)
    (interactive "P")
    (let* ((prompt-dir (consult--directory-prompt "Fd" dir))
           (default-directory (cdr prompt-dir)))
      (find-file (consult--find (car prompt-dir) #'consult--fd-builder initial)))))

(provide 'init-completion)
;;; init-completion.el ends here
