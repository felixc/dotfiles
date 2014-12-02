(defvar skip-whitespace-check nil
  "If non-nil, inhibits behaviour of `maybe-delete-trailing-whitespace', which
  is typically a write-file-hook. This variable may be buffer-local, to permit
  extraneous whitespace on a per-file basis.")
(make-variable-buffer-local 'skip-whitespace-check)

(defun buffer-whitespace-normalized-p ()
  (save-excursion
    (not  (or (progn (beginning-of-buffer)
                     (search-forward "\t" nil t))
              (progn (beginning-of-buffer)
                     (re-search-forward " +$" nil t))))))

(defun whitespace-check-find-file-hook ()
  (unless (buffer-whitespace-normalized-p)
    (message "Disabling whitespace normalization for this buffer...")
    (setq skip-whitespace-check t)))

(setq find-file-hooks
      (cons #'whitespace-check-find-file-hook find-file-hooks))

(defun toggle-whitespace-removal ()
  (interactive)
  (setq skip-whitespace-check (not skip-whitespace-check))
  (message "Whitespace trimming %s"
           (if skip-whitespace-check "disabled" "enabled")))

(defun maybe-delete-trailing-whitespace ()
  (or skip-whitespace-check
      (delete-trailing-whitespace))
  nil)
