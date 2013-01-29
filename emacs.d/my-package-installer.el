(require 'package)

(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(setq my-package-list
  '(ace-jump-mode fill-column-indicator icomplete+ js2-mode pabbrev perspective
    popup undo-tree yasnippet zencoding-mode flymake-cursor zenburn))

(dolist (package my-package-list)
  (when (not (package-installed-p package))
    (when (not package-archive-contents)
      (package-refresh-contents))
    (package-install package)))
