; We don't want GC pauses to slow down interactive operations
(defun disable-gc ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun enable-gc ()
  (setq gc-cons-threshold 3000000))
(add-hook 'minibuffer-setup-hook #'disable-gc)
(add-hook 'minibuffer-exit-hook #'enable-gc)

(disable-gc)

; Override the auto-discovered init file location to try to get packages
; to put their junk in the cache dir instead of ~/.emacs.d
(setq user-emacs-directory "~/.cache/emacs/")

; Don't attempt to resize Emacs on startup to fit default contents. It is
; futile because the tiling window manager controls that anyway.
(setq frame-inhibit-implied-resize t)

; Search for files in a custom load-path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

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

; Identify and adapt to the pre-existing indentation style of the file.
(use-package dtrt-indent
  :defer nil
  :diminish dtrt-indent-mode
  :config
  (dtrt-indent-global-mode))

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

(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

; Change yes/no questions to y/n
(fset 'yes-or-no-p 'y-or-n-p)

; Do not make backups
(setq make-backup-files nil)

; Do not make lockfiles
(setq create-lockfiles nil)

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

; Remove the newline as well when killing a line
(setq kill-whole-line t)

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
(global-set-key (kbd "C-x K") 'kill-buffer)

; Nicer mode line
(use-package powerline
  :defer nil
  :custom
  ;; (setq powerline-height 20)
  ;; (setq powerline-text-scale-factor 0.98)
  (powerline-display-buffer-size nil)
  (powerline-utf-8-separator-left 9625)
  (powerline-utf-8-separator-right 9631)
  (powerline-default-separator 'wave)
  :config
  (powerline-default-theme))

; Better buffer naming when opening multiple files with the same name
(setq uniquify-buffer-name-style 'forward)

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
;; (use-package git-commit
;;   :config
;;   (add-hook 'git-commit-setup-hook (lambda ()
;;     (setq fill-column 76))))

; Save minibuffer history
(use-package savehist
  :custom
  (savehist-mode t))

; Don't bother prompting to end processes when exiting
(add-hook 'comint-exec-hook (lambda ()
  (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

; Better undo
(use-package undo-tree
  :defer nil
  :diminish undo-tree-mode
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.cache/emacs/transient/undo")))
  ; or alternatively:  (setq undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode))

; Automatically use local coding styles defined in .editorconfig files
(use-package editorconfig
  :diminish editorconfig-mode
  :custom (editorconfig-mode 1))

; Keep track of previous positions when moving around and easily jump back.
(use-package backward-forward
  :custom (backward-forward-mode t))

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

; In completion minibuffers, let spaces be spaces and not completion commands.
(define-key minibuffer-local-completion-map " "
    (lambda () (interactive) (insert " ")))

; Match completion candidates flexibly in any order.
(use-package orderless
  :custom (completion-styles '(basic emacs22 partial-completion orderless)))

; Add info to minibuffer completions
(use-package marginalia
  :defer 1
  :config
  (marginalia-mode))

; Incremental mini-buffer completion preview
;; (fido-mode t)
;; (icomplete-mode t)
(fido-vertical-mode t)

; Cycle through all possible completions when hitting tab.
(setq completion-cycle-threshold t)

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
  :defer nil
  :custom
  (zenburn-override-colors-alist
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
      (cond
        ((find-font (font-spec :name "Inconsolata"))
          (set-frame-font "Inconsolata 13" nil t))
        ((find-font (font-spec :name "Monaco"))
          (set-frame-font "Monaco 12" nil t))))))

; Run for already-existing frames, and run when a new frame is created
(mapc 'configure-new-frame-appearance (frame-list))
(add-hook 'after-make-frame-functions 'configure-new-frame-appearance)

; Yasnippet everywhere
(use-package yasnippet-snippets
  :defer 1)
(use-package yasnippet
  :diminish yas-minor-mode
  :defer 1
  :custom (yas-global-mode t))

; Use Flycheck everywhere
(use-package flycheck
  :defer 1
  :diminish flycheck-mode
  :config (global-flycheck-mode))

; Spelling correction, including for comments in programming modes
(use-package flyspell
  :defer 1
  :diminish flyspell-mode
  :diminish flyspell-prog-mode
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . (lambda ()
    (flyspell-mode -1)
    (flyspell-prog-mode)))
  :bind (:map flyspell-mode-map ("C-;" . nil)))

; Better typography in certain modes (curly quotes, dashes, etc)
(use-package typo
  :diminish typo-mode
  :hook
  (git-commit . typo-mode)
  (markdown-mode . typo-mode))

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
  :custom
  (company-tooltip-align-annotations t)
  :config
  (use-package company-quickhelp
    :hook
    (company-mode . company-quickhelp-mode))
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (defun toggle-fci-around-company (command)
    (when (string= "show" command)
      (turn-off-fci-mode))
    (when (string= "hide" command)
      (turn-on-fci-mode)))
  (advice-add 'company-call-frontends :before #'toggle-fci-around-company))

; Email-writing configuration
(require 'my-mail-config)

; Column limit
(setq-default fill-column 80)

; Draw a bar at the fill-column
(use-package fill-column-indicator
  :hook
  (after-change-major-mode . fci-mode)
  :config
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
