;;; init-shell.el --- Config for eshll               -*- lexical-binding: t; -*-

;; Copyright (C) 2023  

;; Author:  <noalias@LAPTOP-G0RSVTIK>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Config for eshell

;;; Code:
(use-package eshell-syntax-highlighting
  :straight t
  :config
  (eshell-syntax-highlighting-global-mode))

(use-package eshell-git-prompt
  :straight t
  :config
  ;; Themes: robbyrussell git-radar powerline simple default
  (eshell-git-prompt-use-theme 'robbyrussell))




(provide 'init-shell)
;;; init-shell.el ends here
