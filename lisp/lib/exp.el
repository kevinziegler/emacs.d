;;No space
;; Comment
;; Not indented from previous line
;;      Indented
;;      also indented
;;         more indent
;; TODO Do some things
;;      More things here

(add-hook 'vertico-mode-hook #'which-key-show-top-level)
(add-hook 'vertico-mode-hook (lambda () (message "Hi Kevin")))
