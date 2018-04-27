; We don't want GC pauses to slow down interactive operations
(defun disable-gc ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun enable-gc ()
  (setq gc-cons-threshold 3000000))
(add-hook 'minibuffer-setup-hook #'disable-gc)
(add-hook 'minibuffer-exit-hook #'enable-gc)

(disable-gc)

; Search for files in a custom load-path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

; Manage packages
(load "my-package-management")

; Set the width of tabs to 2 instead of 8 spaces
(setq tab-width 2)

; But of course, there shouldn't be any tabs
(setq-default indent-tabs-mode nil)

; Set the standard indentation to 2 spaces
(setq-default c-basic-offset 2)

; Indent code in this file with two spaces
(setq lisp-indent-offset 2)

; Auto-break lines in text mode only
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(diminish 'auto-fill-function)

; Sentences are followed by one space only, not two.
(setq sentence-end-double-space nil)

; Display long unbroken lines with the splits at word boundaries
(global-visual-line-mode t)
(diminish 'visual-line-mode)

; No startup message
(setq inhibit-startup-screen t)

; Turn off bell
(setq ring-bell-function 'ignore)

; No toolbar
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

; Hide the right hand side fringe
(fringe-mode '(nil . 0))

; Change yes/no questions to y/n
(fset 'yes-or-no-p 'y-or-n-p)

; Do not make backups
(setq make-backup-files nil)

; Temporary auto-save files go in the temp directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

; Use the X clipboard as well for copy/paste
(setq x-select-enable-clipboard t)
(setq save-interprogram-paste-before-kill t)

; Middle-click paste goes at the point, not where the cursor is
(setq mouse-yank-at-point t)

; Make apropos more comprehensive
(setq apropos-do-all t)

; Do not treat all whitespace as equivalent when searching
(setq search-whitespace-regexp nil)

; Show matching parens
(show-paren-mode t)

; Cycle between moving to the start the line and the first non-whitespace char
(defun move-to-start-of-line ()
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

(global-set-key (kbd "C-a") 'move-to-start-of-line)

; Better buffer management
(global-set-key (kbd "C-x C-b") 'ibuffer)

; We usually want to kill the current buffer.
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x K") 'ido-kill-buffer)

; Nicer mode line
(use-package powerline
  :config
  (setq powerline-height 20)
  (setq powerline-text-scale-factor 0.98)
  (setq powerline-display-buffer-size nil)
  (setq powerline-utf-8-separator-left 9625)
  (setq powerline-utf-8-separator-right 9631)
  (setq powerline-default-separator 'wave)
  (powerline-default-theme))

; Get help with remembering keybindings
(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5)
  (push '(("RET" . nil) . ("↵" . nil)) which-key-replacement-alist)
  (push '(("SPC" . nil) . ("␣" . nil)) which-key-replacement-alist)
  (push '(("TAB" . nil) . ("⇥" . nil)) which-key-replacement-alist)
  (push '(("DEL" . nil) . ("⌫" . nil)) which-key-replacement-alist)
  (push '(("ESC" . nil) . ("⎋" . nil)) which-key-replacement-alist)
  (which-key-mode))

; Better buffer naming for duplicates
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward))

; Move between windows easily
(windmove-default-keybindings)

; When saving a file that looks like a script, make it executable
(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

; Don't ask whether to follow symlinks to version-controlled files, just do it
(setq vc-follow-symlinks t)

; I'll manage my own version control
(setq vc-handled-backends nil)

; Help with writing Git commit messages
(use-package git-commit
  :config
  (add-hook 'git-commit-setup-hook (lambda ()
    (setq fill-column 72))))

; Save minibuffer history
(use-package savehist
  :config
  (savehist-mode t))

; Don't bother prompting to end processes when exiting
(add-hook 'comint-exec-hook (lambda ()
  (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

; Better undo
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

; UTF-8 Unicode
(prefer-coding-system        'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)

; Syntax highlighting
(global-font-lock-mode t)

; Show trailing whitespace and tabs
(setq whitespace-style '(face tabs trailing))
(global-whitespace-mode t)
(diminish 'global-whitespace-mode)

; Delete trailing whitespace in *new* files, but don't mess with existing ones
(require 'my-cond-whitespace-del)
(add-hook 'write-file-hooks 'maybe-delete-trailing-whitespace)

; Interactive-do mode
(use-package ido
  :config
  (ido-mode 'both)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t))

; Incremental mini-buffer completion preview
(eval-after-load "icomplete" '(use-package icomplete+))
(icomplete-mode t)

; Better control over quitting emacs/emacsclient.
(global-unset-key (kbd "C-x C-c"))
(if (daemonp)
  (global-set-key (kbd "C-x ESC") 'delete-frame)
  (global-set-key (kbd "C-x ESC") 'save-buffers-kill-emacs))

; We often have to refresh buffers from disk
(global-set-key (kbd "<f5>") 'revert-buffer)

; Make it easier to get to a dired buffer with the current file at point
(autoload 'dired-jump-other-window "dired-x" "Jump to Dired buffer." t)
(define-key global-map (kbd "C-x C-d") 'dired-jump-other-window)
(add-hook 'dired-mode-hook (lambda ()
  (global-set-key (kbd "C-x C-d") 'dired-jump-other-window)))

; Make word-commands recognize in-word boundaries like capitalization
(global-subword-mode t)
(diminish 'subword-mode)

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
  (setq zenburn-override-colors-alist
    '(("zenburn-bg-2"  . "#000000")
      ("zenburn-bg-1"  . "#101010")
      ("zenburn-bg-05" . "#282828")
      ("zenburn-bg"    . "#2F2F2F")
      ("zenburn-bg+05" . "#383838")
      ("zenburn-bg+1"  . "#3F3F3F")
      ("zenburn-bg+2"  . "#4F4F4F")
      ("zenburn-bg+3"  . "#5F5F5F")))
  :config
  (load-theme 'zenburn t))

; Set the appearance of graphical frames
(defun configure-new-frame-appearance (frame)
  (with-selected-frame frame
    (when (display-graphic-p frame)
      (set-frame-font "Inconsolata 13"))))

; Run for already-existing frames
(mapc 'configure-new-frame-appearance (frame-list))

; Run when a new frame is created
(add-hook 'after-make-frame-functions 'configure-new-frame-appearance)

; Highlight the current line
(global-hl-line-mode)
(set-face-background 'hl-line "#282828")

; Yasnippet everywhere
(use-package yasnippet
  :diminish yas-minor-mode
  :defer 2
  :config (yas-global-mode t))

; Use Flycheck everywhere
(use-package flycheck
  :diminish flycheck-mode
  :config (global-flycheck-mode))

; Spelling correction, including for comments in programming modes
(use-package flyspell
  :diminish flyspell-mode
  :diminish flyspell-prog-mode
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook (lambda ()
    (flyspell-mode -1)
    (flyspell-prog-mode)))
  (define-key flyspell-mode-map (kbd "C-;") nil))

;; Working with Markdown
(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (use-package markdown-toc)
  (custom-set-faces
    '(markdown-bold-face ((t (:inherit basic :weight bold))))
    '(markdown-header-delimiter-face ((t (:inherit font-lock-function-name-face :height 0.8 :foreground "dim gray"))))
    '(markdown-header-face ((t (:inherit font-lock-function-name-face :foreground "cadet blue" :weight bold))))
    '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.6))))
    '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.4))))
    '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.3))))
    '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.2))))
    '(markdown-header-rule-face ((t (:inherit font-lock-function-name-face :foreground "dim gray" :weight bold :height 1.4))))
    '(markdown-italic-face ((t (:inherit basic :slant italic :height 0.9 :family "Source Code Pro"))))))

; Quickly jump to symbols in the buffer
(global-set-key (kbd "M-i") 'imenu)

; Quickly jump around to arbitrary points
(use-package ace-jump-mode
  :bind ("C-l" . ace-jump-mode))

; Refactor by editing multiple regions simultaneously
(use-package iedit
  :bind ("C-;" . iedit-mode))

; Company for completions in various programming languages
(use-package company
  :diminish company-mode
  :config
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (setq company-tooltip-align-annotations t)
  (defun toggle-fci-around-company (command)
    (when (string= "show" command)
      (turn-off-fci-mode))
    (when (string= "hide" command)
      (turn-on-fci-mode)))
  (advice-add 'company-call-frontends :before #'toggle-fci-around-company))

; Language-specific configuration
(require 'my-python-config)
(require 'my-webdev-config)
(require 'my-go-config)
(require 'my-rust-config)

; Email-writing configuration
(require 'my-mail-config)

; Use typopunct in various modes
(require 'my-typopunct-config)

; Jump back to changes
(use-package goto-chg
  :config
  (defun go-to-last-change-and-describe ()
    (interactive)
    (setq current-prefix-arg '(0))
    (call-interactively 'goto-last-change))
  :bind ("M-." . go-to-last-change-and-describe))

; Column limit
(setq-default fill-column 80)

; Draw a bar at the fill-column
(use-package fill-column-indicator
  :config
  (add-hook 'after-change-major-mode-hook 'fci-mode)
  (advice-add 'turn-off-fci-mode :after
    (lambda () (setq fci-mode-toggle nil)))
  ; Toggle the mode as the window resizes around where the line is visible
  (add-hook 'window-configuration-change-hook (lambda ()
    (if (and (bound-and-true-p fci-mode) (<= (window-width) fill-column))
      (progn
        (turn-off-fci-mode)
        (setq fci-mode-toggle t))
      (if (bound-and-true-p fci-mode-toggle) (turn-on-fci-mode))))))

; Helm-Dash for browsing docs
(use-package helm-dash
  :init
  (setq helm-dash-docsets-path "~/.local/share/dash/")
  (add-hook 'emacs-lisp-mode-hook
    (lambda () (setq-local helm-dash-docsets '("Emacs Lisp"))))
  (add-hook 'rust-mode-hook
    (lambda () (setq-local helm-dash-docsets '("Rust"))))
  (add-hook 'html-mode-hook
    (lambda () (setq-local helm-dash-docsets '("HTML"))))
  (add-hook 'python-mode-hook
    (lambda () (setq-local helm-dash-docsets '("Python 3"))))
  :bind
  ("C-h h" . helm-dash)
  ("C-h H" . helm-dash-at-point))

; Writeroom mode for a nicer pure-text writing mode
(use-package writeroom-mode
  :config
  (setq writeroom-extra-line-spacing 0.1)
  (setq writeroom-width 92)
  (defvar in-writeroom-mode)
  (advice-add 'writeroom-mode :after
    (lambda (_)
      (if (bound-and-true-p in-writeroom-mode)
        (progn
          (turn-on-fci-mode)
          (linum-mode 1)
          (global-hl-line-mode)
          (fringe-mode nil)
          (buffer-face-mode -1)
          (set-frame-parameter nil 'internal-border-width 0)
          (makunbound 'in-writeroom-mode))
        (progn
          (turn-off-fci-mode)
          (linum-mode -1)
          (global-hl-line-mode -1)
          (global-hl-line-unhighlight)
          (fringe-mode 0)
          (buffer-face-set '(:height 1.05))
          (set-frame-parameter nil 'internal-border-width 15)
          (setq-local in-writeroom-mode t))))))

; Garbage collection was disabled at the top of this file to speed up startup
(enable-gc)
