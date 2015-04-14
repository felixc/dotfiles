(defun typopunct-setup ()
  "Configure and activate typopunct mode."
  (interactive)
  (require 'typopunct)
  (typopunct-mode)
  (typopunct-change-language 'english t))

(add-hook 'markdown-mode-hook 'typopunct-setup)

(provide 'my-typopunct-config)
