;;;  -*- lexical-binding: t -*-
;;; Global requires
(require 'image-utility)

;;; Global Setting
(setq delete-by-moving-to-trash t)       ; Deleting files go to OS's trash folder

;;; dired
(use-package dired
  :bind
  ("C-x d" . dired-jump-other-window)
  ("C-x C-d" . dired)
  (:map dired-mode-map
        ;; 折叠子目录
        ("TAB" . dired-hide-subdir)
        ("C-k" . dired-kill-subdir)
        ("M-p" . dired-prev-subdir)
        ("M-n" . dired-next-subdir)
        ;; `f' 进入目录或文件
        ;; `b' 返回上级目录
        ("b" . dired-up-directory)
        ("e" . dired:find-file-externally)
        ("/ p" . dired:convert-image-to-pdf)
        ("/ i" . dired:convert-pdf-to-image)
        ("/ m" . dired:merge-pdf-files))
  :config
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group"
        dired-recursive-deletes 'always
        dired-recursive-copies 'always
        ;;        dired-kill-when-opening-new-dired-buffer t
        )
  (setq dired-guess-shell-alist-user
        `(("elvish tar.elv")
          ("\\.elv\\'" "elvish")
          ("\\.pdf\\'" "mupdf &")
          (,(rx bos ?.
                (or (seq "do" (or ?c ?t) (? ?x))
                    "ppt")
                eos)
           "wps &")))
  
  (defun dired:find-file-externally (&optional arg)
    "Open marked or current file in operating system's default application."
    (interactive "P")
    (dired-map-over-marks
     (consult-file-externally (dired-get-file-for-visit))
     arg))

  (defun dired:merge-pdf-files (name)
    "将 `image' 文件及 `pdf' 合并为一个 `pdf' 文件"
    (interactive "sOutput file name: ")
    (let ((files (dired-get-marked-files))
          (default-directory (dired-current-directory)))
      (if (length< files 2)
          (user-error "Less files to merge"))
      (apply #'image-utility--merge-files name
             (mapcar (lambda (file)
                       (pcase (file-name-extension file)
                         ((or "png" "pdf") file)
                         (_ (image-utility--convert file))))
                     files))))

  (defun dired:convert-image-to-pdf (&optional arg)
    "将 `image' 文件转化为 pdf 文件"
    (interactive "P")
    (let ((default-directory (dired-current-directory)))
      (dired-map-over-marks
       (image-utility--convert (dired-get-filename) "pdf")
       arg)))

  (defun dired:convert-pdf-to-image (&optional arg)
    "将 `pdf' 文件转化为 `image' 文件"
    (interactive "P")
    (let ((default-directory (dired-current-directory)))
      (dired-map-over-marks
       (image-utility--convert (dired-get-filename) "png")
       arg)))
  )

(use-package dired-x
  :hook (dired-mode-hook . dired-omit-mode)
  :config
  (setq dired-omit-files
        (rx bos (or (seq "desktop.ini")
                    (seq ?~ (? ?$) (* (or alnum (category chinese-two-byte))) (? ".tmp"))
                    eos))))

(use-package diredfl
  :straight t
  :after dired
  :hook
  ((dired-mode . diredfl-mode)
   (dirvish-directory-view-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(use-package dirvish
  :straight t
  :hook (after-init-hook . dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "/mnt/"                       "Drives")
     ("t" "~/.local/share/Trash/files/" "TrashCan")))
                                        ;(dirvish-path-separators '(" ^" " /" " > "))
  :config
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(all-the-icons file-time file-size subtree-state))
  (setq dirvish-use-header-line 'global)
  
  (dirvish-define-preview exa (file)
    "Use `exa' to generate directory preview."
    :require ("exa") ; tell Dirvish to check if we have the executable
    (when (file-directory-p file) ; we only interest in directories here
      `(shell . ("exa" "-al" "--color=always" "--icons"
                 "--group-directories-first" ,file))))
  (if (and base:linux-p
           (executable-find "exa"))
      (add-to-list 'dirvish-preview-dispatchers 'exa))
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("M-c f" . dirvish-fd)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("SPC" . dirvish-dispatch)
   ("t" . dirvish-layout-toggle)
   ("a"   . dirvish-quick-access)
   ("F"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

(provide 'init-dired)
;;; init-dired.el ends here
