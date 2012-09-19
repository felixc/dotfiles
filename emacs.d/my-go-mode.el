(require 'go-mode-load)

(add-hook 'go-mode-hook
  (lambda ()
    (setq tab-width 2)
    (toggle-show-tabs-show-ws)    ; Toggled twice to work around a bug either in
    (toggle-show-tabs-show-ws)))  ; the function or in my brain.
                                  ; Starts globally on, toggling once keeps it
                                  ; on, twice turns it off.
