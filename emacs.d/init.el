; Search for files in a custom load-path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

; Verify TLS certificates
(setq gnutls-verify-error t)

; Indent code in this file with two spaces
(setq lisp-indent-offset 2)

; Manage packages
(load "my-package-management.el")

; Auto-break lines in text mode only
(add-hook 'text-mode-hook 'turn-on-auto-fill)

; Display long unbroken lines with the splits at word boundaries
(global-visual-line-mode t)

; No startup message
(setq inhibit-startup-message t)

; Turn off bell
(setq ring-bell-function 'ignore)

; No toolbar or scrollbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

; Show matching parens
(show-paren-mode t)

; Syntax highlighting
(global-font-lock-mode t)

; Set the width of tabs to 2 instead of 8 spaces
(setq tab-width 2)

; But of course, there shouldn't be any tabs
(setq-default indent-tabs-mode nil)

; Set the standard indentation to 2 spaces
(setq-default c-basic-offset 2)

; Show trailing whitespace and tabs
(setq whitespace-style '(face tabs trailing))
(global-whitespace-mode t)

; Delete trailing whitespace in *new* files, but don't mess with existing ones
(require 'my-cond-whitespace-del)
(add-hook 'write-file-hooks 'maybe-delete-trailing-whitespace)

; Change yes/no questions to y/n
(fset 'yes-or-no-p 'y-or-n-p)

; Do not make backups
(setq make-backup-files nil)

; Temporary auto-save files go in the temp directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

; Don't ask whether to follow symlinks to version-controlled files, just do it
(setq vc-follow-symlinks t)

; Use the X clipboard as well for copy/paste
(setq x-select-enable-clipboard t)
(setq save-interprogram-paste-before-kill t)

; Middle-click paste goes at the point, not where the cursor is
(setq mouse-yank-at-point t)

; Make apropos more comprehensive
(setq apropos-do-all t)

; Interactive-do mode
(use-package ido
  :demand t
  :config
  (ido-mode 'both)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t))

; Incremental mini-buffer completion preview
(eval-after-load "icomplete" '(progn (use-package icomplete+)))
(icomplete-mode t)

; Better buffer management
(global-set-key (kbd "C-x C-b") 'ibuffer)

; We usually want to kill the current buffer.
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x K") 'ido-kill-buffer)

; Better buffer naming for duplicates
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward))

; Move between windows easily
(windmove-default-keybindings)

; Quickly jump to symbols in the buffer
(global-set-key (kbd "M-i") 'imenu)

; Quickly jump around to arbitrary points
(use-package ace-jump-mode
  :bind ("C-l" . ace-jump-mode))

; Refactor by editing multiple regions simultaneously
(use-package iedit)

; Better undo
(use-package undo-tree
  :config
  (global-undo-tree-mode))

; Avoid accidentally killing emacs all the time
(global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-x ESC") 'save-buffers-kill-emacs)

; We often have to refresh buffers from disk
(global-set-key (kbd "<f5>") 'revert-buffer)

; Make it easier to get to a dired buffer with the current file at point
(autoload 'dired-jump-other-window "dired-x" "Jump to Dired buffer." t)
(define-key global-map (kbd "C-x C-d") 'dired-jump-other-window)
(add-hook 'dired-mode-hook
  (lambda ()
    (global-set-key (kbd "C-x C-d") 'dired-jump-other-window)))

; Spelling correction, including for comments in programming modes
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook
  (lambda ()
    (flyspell-mode -1)
    (flyspell-prog-mode)))

; Make word-commands recognize in-word boundaries like capitalization
(global-subword-mode t)

; Make the selection behave like in most other applications
(delete-selection-mode t)

; Line numbering
(use-package linum
  :config
  (global-linum-mode t)
  (setq column-number-mode t))

; Scroll only one line at a time
(setq scroll-step 1)
(setq mouse-wheel-scroll-amount '(1))

; Mouse wheel scrolls whatever window the mouse is over
(setq mouse-wheel-follow-mouse t)

; Appearance
(use-package zenburn-theme
  :init
  (defvar zenburn-override-colors-alist
    '(("zenburn-bg-2"     . "#000000")
      ("zenburn-bg-1"     . "#101010")
      ("zenburn-bg-05"    . "#282828")
      ("zenburn-bg"       . "#2F2F2F")
      ("zenburn-bg+05"    . "#383838")
      ("zenburn-bg+1"     . "#3F3F3F")
      ("zenburn-bg+2"     . "#4F4F4F")
      ("zenburn-bg+3"     . "#5F5F5F")))
  (load-theme 'zenburn t))
(set-frame-font "Inconsolata-13")

; Highlight the current line
(global-hl-line-mode)
(set-face-background 'hl-line "#282828")

; Yasnippet everywhere
(yas-global-mode t)

; Use Flycheck everywhere
(global-flycheck-mode)

; Recognize python3 files
(add-to-list 'interpreter-mode-alist '("python3" . python-mode))

; Recognize Markdown files
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

; Zencoding for SGML modes
(add-hook 'sgml-mode-hook 'zencoding-mode)

; Better JS and CSS editing in HTML documents
(use-package multi-web-mode
  :config
  (setq mweb-default-major-mode 'html-mode)
  (setq mweb-tags '((js-mode "<script[^>]*>" "</script>")
                     (css-mode "<style[^>]*>" "</style>")))
  (setq mweb-filename-extensions '("html"))
  (multi-web-global-mode t))

; JS2 Mode
(require 'my-js-mode)

; Go mode
(require 'my-go-mode)

; Rust mode
(require 'my-rust-mode)

; Mail mode
(require 'my-mail-mode)

; Use typopunct in various modes
(require 'my-typopunct-config)

; Show colours as represented by strings
(add-hook 'css-mode-hook 'rainbow-mode)

; UTF-8 Unicode
(prefer-coding-system        'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)

; Jump back to changes
(use-package goto-chg
  :config
  (global-set-key (kbd "M-.") '(lambda ()
     (interactive)
     (setq current-prefix-arg '(0))
     (call-interactively 'goto-last-change))))

; Column limit
(setq-default fill-column 80)

; Draw a bar at the fill-column
(use-package fill-column-indicator
  :config
  (add-hook 'after-change-major-mode-hook 'fci-mode)
  (advice-add 'turn-off-fci-mode :after
    #'(lambda () (setq fci-mode-toggle nil)))
  ; Toggle the mode as the window resizes around where the line is visible
  (add-hook 'window-configuration-change-hook
    (lambda ()
      (if (and (bound-and-true-p fci-mode) (<= (window-width) fill-column))
        (progn
          (turn-off-fci-mode)
          (setq fci-mode-toggle t))
         (if (bound-and-true-p fci-mode-toggle) (turn-on-fci-mode))))))

; Org Mode customizations
(require 'my-org-mode)

; Ignore useless file extensions
(add-to-list 'completion-ignored-extensions ".pyc")
(add-to-list 'completion-ignored-extensions ".6")
(add-to-list 'completion-ignored-extensions ".8")
(add-to-list 'completion-ignored-extensions ".out")

; Auto completion
(use-package pabbrev
  :config
  (global-pabbrev-mode t))

(use-package popup
  :config
  (define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
  (defun pabbrevx-suggestions-goto-buffer (suggestions)
    (let* ((candidates (mapcar 'car suggestions))
            (bounds (pabbrev-bounds-of-thing-at-point))
            (selection (popup-menu* candidates :point (car bounds) :scroll-bar t)))
      (when selection
        (let ((point))
          (save-excursion
            (progn
              (delete-region (car bounds) (cdr bounds))
              (insert selection)
              (setq point (point))))
          (if point
            (goto-char point))
          (setq pabbrev-last-expansion-suggestions nil)))))
  (fset 'pabbrev-suggestions-goto-buffer 'pabbrevx-suggestions-goto-buffer))

; When saving a file that looks like a script, make it executable
(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

; Save minibuffer history
(use-package savehist
  :config
  (savehist-mode t))

; Save open files on close, reopen them lazily at startup if wanted
(setq
  desktop-restore-eager 5
  desktop-save t
  desktop-load-locked-desktop t
  desktop-dirname (expand-file-name "~/.emacs.d")
  desktop-path (list desktop-dirname))
(if (= (length command-line-args) 1)
    (desktop-save-mode t))

; Don't bother prompting to end processes when exiting
(add-hook 'comint-exec-hook
  (lambda ()
    (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

; Use C-x k to end emacsclient sessions, rather than C-x #
(add-hook 'server-switch-hook
  (lambda ()
    (when (current-local-map)
      (use-local-map (copy-keymap (current-local-map))))
    (local-set-key (kbd "C-x k") 'server-edit)))
