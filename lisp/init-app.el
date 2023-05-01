;;; init-app.el --- -*- lexical-binding: t -*-
(use-package elfeed
  :straight t
  :config
  ;; Somewhere in your .emacs file
  (setq elfeed-feeds
        '("http://nullprogram.com/feed/"
          "https://planet.emacslife.com/atom.xml")))

;;; Tails
(provide 'init-app)
