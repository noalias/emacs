;;;  -*- lexical-binding: t -*-
(use-package emacs
  :init
  ;; config font
  (defvar face:font-size 25)

  (defvar face:default-fonts ["Fairfax HD" "FantasqueSansMono NFM"]
    "My fonts.")

  (defvar face:emoji-font
    (cond (base:win-p "Segoe UI Emoji")
          (base:linux-p "Noto Color Emoji")))

  (defvar face:fontset-font "HarmonyOS Sans SC")
  :config
  (progn ; `font'
    (setq inhibit-compacting-font-caches t)  ; Don’t compact font caches during GC.
    (set-face-attribute 'default
		                nil
		                :font (font-spec :family (aref face:default-fonts 0)
				                         :size face:font-size))
    (set-fontset-font t
		              'unicode
		              (font-spec :family face:emoji-font
			                     :size face:font-size))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font
       (frame-parameter nil 'font)
       charset
       (font-spec :family face:fontset-font
                  :size face:font-size)))))

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-one :no-confirm)
  (setq doom-themes-enable-bold t
	    doom-themes-enable-italic t))

(use-package doom-modeline
  :straight t
  :hook after-init-hook
  :config
  (setq doom-modeline-major-mode-icon t
        dirvish-mode-line-height doom-modeline-height))

(provide 'init-face)
;;; init-face.el ends here
