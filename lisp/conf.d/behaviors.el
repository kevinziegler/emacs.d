(setq switch-to-buffer-obey-display-actions t)

(add-to-list 'display-buffer-alist
             `(,(lambda (buffer _)
                  (with-current-buffer buffer
                    (derived-mode-p 'comint-mode)))
               display-buffer-in-side-window
               (side . bottom)
               (slot . 99)
               (dedicated . t)))

(add-to-list 'display-buffer-alist
             `(,(lambda (buffer _)
                  (with-current-buffer buffer
                    (derived-mode-p 'help-mode 'helpful-mode)))
               display-buffer-in-side-window
               (side . right)
               (dedicated . t)))

(add-to-list 'display-buffer-alist
             '("\\*Ibuffer\\*"
               display-buffer-in-tab
               (tab-name . "Buffers")))

(add-to-list 'display-buffer-alist
             '("\\*Messages\\*"
               display-buffer-in-tab
               (tab-name . "System")))

(add-to-list 'display-buffer-alist
             '("\\*Warnings\\*"
               display-buffer-in-tab
               (tab-name . "System")))

(add-to-list 'display-buffer-alist
             '("\\*lsp-install: .+\\*"
               display-buffer-in-tab
               (tab-name . "System")))

(add-to-list 'display-buffer-alist
             '("\\*straight-process\\*"
               display-buffer-in-tab
               (tab-name . "Packages")))

(add-to-list 'display-buffer-alist
             '("\\*Packages\\*"
               display-buffer-in-tab
               (tab-name . "Packages")))

(add-to-list 'display-buffer-alist
             '("\\*dashboard\\*"
               display-buffer-in-tab
               (tab-name . "Home")))

(add-to-list 'display-buffer-alist
             '("\\*Embark Collect: .+\\*"
               display-buffer-in-side-window
               (side . bottom)
               (slot . 99)
               (dedicated . t)))

(provide 'conf.d/behaviors)
