;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Early birds
(progn ;     startup
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (setq inhibit-splash-screen 1)
  )

(progn ;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

(defconst straight-base-dir (or (getenv "XDG_CACHE_HOME") user-emacs-directory))
(eval-and-compile ;; `straight'
  (let* ((default-directory
          (expand-file-name "straight/repos" straight-base-dir))
         (bootstrap-file
          (expand-file-name "straight.el/bootstrap.el")))
    (unless (file-exists-p bootstrap-file)
      (or (file-exists-p default-directory) (mkdir default-directory :parents))
      (process-lines "git"
                     "clone"
                     "-b"
                     "develop"
                     "https://github.com/radian-software/straight.el.git"
                     "--depth"
                     "1"))
    (load bootstrap-file nil :nomessage))

  (setq straight-vc-git-default-clone-depth 1))

(eval-and-compile ;; `use-package'
  (straight-use-package '(use-package :type built-in))
  ;; `use-package' disable omit `-hook'
  (setq use-package-hook-name-suffix nil)
  (setq use-package-verbose t))

;;; Long tail
(use-package emacs
  :load-path ("lisp" "site-lisp")
  :config
  (use-package init-base)
  (use-package init-completion)
  (use-package init-input-method)
  (use-package init-face)
  (use-package init-text)
  (use-package init-prog)
  (use-package init-help)
  (use-package init-project)
  (use-package init-file)
  (use-package init-buffer)
  (use-package init-window)
  (use-package init-dired)
  (use-package init-edit)
  (use-package init-shell)
  )

;;; Tequila worms

(progn ;     startup
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'after-init-hook
            (lambda ()
              (message
               "Loading %s...done (%.3fs) [after-init]" user-init-file
               (float-time (time-subtract (current-time)
                                          before-user-init-time))))
            t))

(progn ;     personalize
  (let ((file (expand-file-name (concat (user-real-login-name) ".el")
                                user-emacs-directory)))
    (when (file-exists-p file)
      (load file))))

;;; init.el ends here
;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
