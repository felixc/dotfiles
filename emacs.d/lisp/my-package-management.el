;;; my-package-management.el -- Install and manage the packages I want to use

(require 'package)

(setq package-archives '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(defvar my-package-list
  '(ace-jump-mode company fill-column-indicator flycheck goto-chg helm-dash
    iedit icomplete+ js2-mode multi-web-mode pabbrev rainbow-mode racer
    rust-mode typopunct undo-tree use-package writeroom-mode yasnippet
    zenburn-theme))

(dolist (package my-package-list)
  (when (not (package-installed-p package))
    (when (not package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(require 'use-package)
(setq use-package-always-ensure t)

(provide 'my-package-management)
