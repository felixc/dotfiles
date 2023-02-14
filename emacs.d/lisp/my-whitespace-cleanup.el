(defvar skip-trailing-whitespace-deletion nil
  "Control execution of `maybe-delete-trailing-whitespace'.
`maybe-delete-trailing-whitespace' is typically a write-file-hook. This variable
may be buffer-local, to allow unclean whitespace on a per-file basis.")
(make-variable-buffer-local 'skip-trailing-whitespace-deletion)

(defun toggle-trailing-whitespace-deletion ()
  "Enable/disable trailing whitespace removal on save."
  (interactive)
  (setq skip-trailing-whitespace-deletion (not skip-trailing-whitespace-deletion)))

(defun maybe-delete-trailing-whitespace ()
  "Delete trailing whitespace if and only if the buffer is configured to do so."
  (unless skip-trailing-whitespace-deletion
    (delete-trailing-whitespace)))


(defun show-trailing-whitespace ()
  "Highlight trailing whitespace regardless of whether it's preexisting."
  (interactive)
  (add-to-list (make-local-variable 'whitespace-style) 'trailing)
  (whitespace-mode 1))


(defun buffer-has-trailing-whitespace-p ()
  "Does the buffer already contain trailing whitespace?"
  (save-excursion
    (progn (goto-char (point-min))
           (re-search-forward "[ \t]+$" nil t))))

(defun buffer-has-tabs-p ()
  "Does the buffer already contain tabs?"
  (save-excursion
    (progn (goto-char (point-min))
           (search-forward "\t" nil t))))

(defun my-whitespace-cleanup-configuration-hook ()
  "Configures whitespace visibility and cleanup."
  (if (buffer-has-trailing-whitespace-p)
    (setq skip-trailing-whitespace-deletion t)
    (add-to-list (make-local-variable 'whitespace-style) 'trailing))
  (add-to-list (make-local-variable 'whitespace-style) (if (buffer-has-tabs-p) 'tab-mark 'tabs))
  (whitespace-mode 1))


; Automatically configure whitespace management on files.
(add-hook 'find-file-hook 'my-whitespace-cleanup-configuration-hook 90)
(add-hook 'write-file-functions 'maybe-delete-trailing-whitespace)


; Show trailing whitespace and tabs
(setq whitespace-style '(face space-before-tab))
(global-whitespace-mode)
(diminish 'global-whitespace-mode)
(diminish 'whitespace-mode)


(provide 'my-whitespace-cleanup)
