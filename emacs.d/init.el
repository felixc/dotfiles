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

; Keep customizations in a separate file
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

; Manage packages
(require 'my-package-management)

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

; No app chrome (toolbar, menu, scrollbar)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)

; Hide the right hand side fringe
(fringe-mode '(nil . 0))

; Change yes/no questions to y/n
(fset 'yes-or-no-p 'y-or-n-p)

; Do not make backups
(setq make-backup-files nil)

; Do not make lockfiles
(setq create-lockfiles nil)

; Temporary auto-save files go in the temp directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

; Use the X clipboard as well for copy/paste
(setq select-enable-clipboard t)
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

; Select entire words, sentences, paragraphs... (tokens, expressions...)  at a time
(use-package expand-region
  :bind ("C-=" . er/expand-region))

; Better buffer management
(global-set-key (kbd "C-x C-b") 'ibuffer)

; We usually want to kill the current buffer.
(global-set-key (kbd "C-x k") 'kill-current-buffer)
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
    (setq fill-column 76))))

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

; Automatically use local coding styles defined in .editorconfig files
(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

; UTF-8 Unicode
(prefer-coding-system        'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)

; Syntax highlighting
(global-font-lock-mode t)

; Highlight and delete unwanted whitespace
(require 'my-whitespace-cleanup)

; Interactive-do mode
(use-package ido
  :config
  (ido-mode 'both)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t))

; Incremental mini-buffer completion preview
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
(global-display-line-numbers-mode)

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

; Highlight the current line
(global-hl-line-mode)
(set-face-background 'hl-line "#282828")

; Helper function to set the appearance of graphical frames
(defun configure-new-frame-appearance (frame)
  (with-selected-frame frame
    (when (display-graphic-p frame)
      (set-frame-font "Inconsolata 13"))))

; Run for already-existing frames, and run when a new frame is created
(mapc 'configure-new-frame-appearance (frame-list))
(add-hook 'after-make-frame-functions 'configure-new-frame-appearance)

; Yasnippet everywhere
(use-package yasnippet-snippets
  :defer 2)
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

; Better typography in certain modes (curly quotes, dashes, etc)
(use-package typo
  :diminish typo-mode
  :config
  (add-hook 'git-commit-mode 'typo-mode)
  (add-hook 'markdown-mode-hook 'typo-mode)
  (typo-global-mode))

;; Working with Markdown
(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (use-package markdown-toc)
  (custom-set-faces
    '(markdown-bold-face ((t (:inherit basic :weight bold))))
    '(markdown-code-face ((t (:inherit basic))))
    '(markdown-header-delimiter-face ((t (:inherit font-lock-function-name-face :height 1.0 :foreground "dim gray"))))
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
(use-package avy
  :bind ("C-l" . avy-goto-char))

; Refactor by editing multiple regions simultaneously
(use-package iedit
  :bind ("C-;" . iedit-mode))

; Company for completions in various programming languages
(use-package company
  :diminish company-mode
  :config
  (use-package company-quickhelp
    :config
    (add-hook 'company-mode-hook 'company-quickhelp-mode))
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (setq company-tooltip-align-annotations t)
  (defun toggle-fci-around-company (command)
    (when (string= "show" command)
      (turn-off-fci-mode))
    (when (string= "hide" command)
      (turn-on-fci-mode)))
  (advice-add 'company-call-frontends :before #'toggle-fci-around-company))

; Language-specific configuration
(require 'my-language-server-config)
(require 'my-python-config)
(require 'my-webdev-config)
(require 'my-go-config)
(require 'my-rust-config)
(require 'my-typescript-config)

; Email-writing configuration
(require 'my-mail-config)

; Jump back to changes
(use-package goto-chg
  :config
  (defun go-to-last-change-and-describe ()
    (interactive)
    (setq current-prefix-arg '(0))
    (call-interactively 'goto-last-change))
  :bind ("C-M-/" . go-to-last-change-and-describe))

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

; Garbage collection was disabled at the top of this file to speed up startup
(enable-gc)
