; Search for files in a custom load-path
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

; Extra package repos
(require 'package)
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

; Auto-break lines in text mode only
(add-hook 'text-mode-hook 'turn-on-auto-fill)

; Display long unbroken lines with the splits at word boundaries
(global-visual-line-mode t)

; No startup message
(setq inhibit-startup-message t)

; Turn off bell
(setq ring-bell-function 'ignore)

; No toolbar or scrollbar
(tool-bar-mode -1)
(scroll-bar-mode -1)

; Show matching parens
(show-paren-mode t)

; Syntax highlighting
(global-font-lock-mode t)

; Set the width of tabs to 2 instead of 8 spaces
(setq tab-width 2)

; But of course, there shouldn't be any tabs
(setq-default indent-tabs-mode nil)

; Set the standard indentation to 2 spaces
(setq c-basic-offset 2)

; Show trailing whitespace
(setq-default show-trailing-whitespace t)

; Delete trailing whitespace in *new* files, but don't mess with existing ones
(load "my-cond-whitespace-del")
(add-hook 'write-file-hooks 'maybe-delete-trailing-whitespace)

; Change yes/no questions to y/n
(fset 'yes-or-no-p 'y-or-n-p)

; Do not make backups
(setq make-backup-files nil)

; Interactive-do mode
(require 'ido)
(ido-mode 'both)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

; Incremental mini-buffer completion preview
(eval-after-load "icomplete" '(progn (require 'icomplete+)))
(icomplete-mode t)

; Better buffer management
(global-set-key (kbd "C-x C-b") 'ibuffer)

; Move between windows easily
(windmove-default-keybindings)

; Quickly jump to symbols in the buffer
(global-set-key (kbd "M-i") 'imenu)

; Quickly jump around to arbitrary points
(require 'ace-jump-mode)
(global-set-key (kbd "C-l") 'ace-jump-mode)

; Better undo
(require 'undo-tree)
(global-undo-tree-mode)

; Clean up unused junk
(require 'midnight)
(setq clean-buffer-list-delay-general 0)

; Avoid accidentally killing emacs all the time
(global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-x <escape>") 'save-buffers-kill-emacs)

; We often have to refresh buffers from disk
(global-set-key (kbd "<f5>") 'revert-buffer)

; Show tabs, so that they can be destroyed
(require 'show-wspace)
(add-hook 'font-lock-mode-hook 'show-ws-highlight-tabs)

; Make word-commands recognize in-word boundaries like capitalization
(global-subword-mode t)

; Make the selection behave like in most other applications
(delete-selection-mode t)

; Auto completion
(require 'pabbrev)
(global-pabbrev-mode)

(require 'popup)
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

(fset 'pabbrev-suggestions-goto-buffer 'pabbrevx-suggestions-goto-buffer)

; Line numbering
(require 'linum)
(global-linum-mode t)
(setq column-number-mode t)

; Appearance
(require 'zenburn)
(color-theme-zenburn)
(set-default-font "Inconsolata-13")

; Highlight the current line
(global-hl-line-mode)
(set-face-background 'hl-line "#0f0f0f")
(set-face-underline-p 'hl-line nil)

; JS2 Mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook '(lambda () (setq fill-column 100)))

; Haskell Mode
(require 'haskell-mode)
(setq haskell-font-lock-symbols t)

; Go mode
(load "my-go-mode")

; Common Lisp / Slime
(require 'cl)
(setq inferior-lisp-program "/usr/bin/sbcl")

; UTF-8 Unicode
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

; Use perspectives (workspaces)
(require 'perspective)
(persp-mode)

; Column limit
(setq-default fill-column 80)

; Draw a bar at the fill-column
(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

; Org Mode customizations
(load "my-org-mode")

; Ignore useless file extensions
(add-to-list 'completion-ignored-extensions ".pyc")
(add-to-list 'completion-ignored-extensions ".6")
(add-to-list 'completion-ignored-extensions ".8")
(add-to-list 'completion-ignored-extensions ".out")

; When saving a file that looks like a script, make it executable
(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

; Save open files on close, reopen them lazily at startup if wanted
(setq
  desktop-restore-eager 5
  desktop-save t
  desktop-load-locked-desktop t
  desktop-dirname (expand-file-name "~/.emacs.d")
  desktop-path (list desktop-dirname))
(if (= (length command-line-args) 1)
    (desktop-save-mode 1))

; Use C-x k to end emacsclient sessions, rather than C-x #
(add-hook 'server-switch-hook
  (lambda ()
    (when (current-local-map)
      (use-local-map (copy-keymap (current-local-map))))
    (local-set-key (kbd "C-x k") 'server-edit)))

(server-start)
