(add-hook 'org-insert-heading-hook #'kdz/org-heading-fixup-new-line)

(add-hook 'org-mode-hook
          (lambda ()
            (add-function :before-until
                          (local 'electric-pair-inhibit-predicate)
                          (lambda (c) (eq c ?<)))))
