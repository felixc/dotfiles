(require 'package)

(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(setq my-package-list
  '(ace-jump-mode fill-column-indicator flycheck goto-chg iedit icomplete+
    js2-mode multi-web-mode pabbrev perspective popup rainbow-mode rust-mode
    undo-tree yasnippet zencoding-mode zenburn))

(dolist (package my-package-list)
  (when (not (package-installed-p package))
    (when (not package-archive-contents)
      (package-refresh-contents))
    (package-install package)))
