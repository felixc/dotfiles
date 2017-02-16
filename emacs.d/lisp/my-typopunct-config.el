(use-package typopunct
  :diminish typopunct-mode
  :config
  (defun typopunct-setup ()
    (interactive)
    (typopunct-mode)
    (typopunct-change-language 'english t))
  (add-hook 'markdown-mode-hook 'typopunct-setup))

(provide 'my-typopunct-config)
