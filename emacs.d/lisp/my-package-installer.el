(require 'package)

(setq package-archives '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(setq my-package-list
  '(ace-jump-mode fill-column-indicator flycheck goto-chg iedit icomplete+
    js2-mode multi-web-mode pabbrev perspective popup rainbow-mode rust-mode
    typopunct undo-tree yasnippet zencoding-mode zenburn))

(dolist (package my-package-list)
  (when (not (package-installed-p package))
    (when (not package-archive-contents)
      (package-refresh-contents))
    (package-install package)))
