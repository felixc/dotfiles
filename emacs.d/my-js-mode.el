(require 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook '(lambda () (setq fill-column 100)))

(when (load "flymake" t)
  (defun flymake-closure-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "java"
            (list "-jar" (expand-file-name "~/bin/closure.jar")
                  "--warning_level" "VERBOSE"
                  "--js" local-file
                  "--js_output_file" "/dev/null"))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.js\\'" flymake-closure-init)))

(add-hook 'js2-mode-hook 'flymake-mode)

