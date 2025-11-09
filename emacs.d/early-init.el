;;; early-init --- Early pre-loading customizations.

;; If an '.el' file is newer than its corresponding '.elc', load the '.el'.
(setq load-prefer-newer t)

;; Store installed Elisp packages in the cache dir instead of ~/.emacs.d
(setq package-user-dir "~/.cache/emacs/elpa")

;; Put native compilation caches in the cache dir instead of ~/.emacs.d
(startup-redirect-eln-cache "~/.cache/emacs/eln-cache")

;; Put auto-save metadata in cache dir instead of ~/.emacs.d
(setq auto-save-list-file-prefix "~/.cache/emacs/auto-save-list/.saves-")

;; Write any customizations to a temp file so they are discarded.
(setq custom-file (make-temp-file "custom-" nil ".el"))

;; Don't warn about packages being compiled in the background.
(setq native-comp-async-report-warnings-errors nil)

;; Temporarily unset 'file-name-handler-alist'. Every file loaded by Emacs will
;; go through this list looking for a handler for the file, but during startup
;; we don't need any of them.
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist file-name-handler-alist-original)))
