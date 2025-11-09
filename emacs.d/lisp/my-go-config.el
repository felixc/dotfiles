(use-package go-mode
  :mode "\\.go\\'"
  :hook
  (go-mode . (lambda ()
    (setq tab-width 2)
    (toggle-show-tabs-show-ws)     ; Toggled twice to work around a bug either
    (toggle-show-tabs-show-ws))))  ; in the function or in my brain.
                                   ; Starts globally on, toggling once keeps it
                                   ; on, twice turns it off.
(provide 'my-go-config)
