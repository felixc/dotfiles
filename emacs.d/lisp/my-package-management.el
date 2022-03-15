(require 'package)

(setq gnutls-verify-error t)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package--init-file-ensured t)
(setq package-enable-at-startup nil)

(package-initialize)

(unless (package-installed-p 'use-package)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

(setq use-package-always-ensure t)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(provide 'my-package-management)
