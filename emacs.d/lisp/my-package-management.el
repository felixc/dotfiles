(require 'package)

(setq gnutls-verify-error t)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package--init-file-ensured t)
(setq package-enable-at-startup nil)


(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

(setq use-package-always-defer t)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :defer nil
  :custom
    (auto-package-update-delete-old-versions t)
    (auto-package-update-hide-results t)
    (auto-package-update-last-update-day-filename "~/.cache/emacs/last-package-update-day")
  :config
    (auto-package-update-maybe))

(provide 'my-package-management)
