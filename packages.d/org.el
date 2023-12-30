(use-package org
  :config
  (setopt org-auto-align-tags nil
          org-tags-column 0
          org-fold-catch-invisible-edits 'show-and-error
          org-special-ctrl-a/e t
          org-insert-heading-respect-content t
          ;; Org styling, hide markup etc.
          org-hide-emphasis-markers t
          org-pretty-entities t
          org-ellipsis "…"
          ;; Agenda styling
          org-agenda-tags-column 0
          org-agenda-block-separator ?─
          org-agenda-time-grid
          '((daily today require-timed)
            (800 1000 1200 1400 1600 1800 2000)
            " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
          org-agenda-current-time-string
          "⭠ now ─────────────────────────────────────────────────")

  (defun kdz/org-heading-fixup-new-line ()
    "Ensure an empty line between non-empty org-mode headings"
    (when (kdz/org-point-is-heading-p)
      (save-excursion
        (previous-line)
        (let ((previous (thing-at-point 'line)))
          (when (and previous
                     (not (string-match-p "^\*+ .*$" previous))
                     (not (string= "\n" previous)))
            (goto-char (line-end-position))
            (insert "\n"))))))

  (add-hook 'org-insert-heading-hook #'kdz/org-heading-fixup-new-line)
  (add-hook 'org-mode-hook
            (lambda ()
              (add-function :before-until
                            (local 'electric-pair-inhibit-predicate)
                            (lambda (c) (eq c ?<)))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)
     (sed . t)
     (lua . t)
     (js . t)
     (plantuml . t)
     (python . t)
     (sql . t)
     (sqlite . t)
     (emacs-lisp .t))))

(use-package org-modern
  :straight t
  :after org
  :config
  (global-org-modern-mode))

(use-package org-appear
  :straight t
  :after org
  :config
  (setopt org-appear-trigger 'manual)

  (add-hook 'org-mode-hook 'org-appear-mode)
  (add-hook 'org-mode-hook (lambda ()
                           (add-hook 'evil-insert-state-entry-hook
                                     #'org-appear-manual-start
                                     nil
                                     t)
                           (add-hook 'evil-insert-state-exit-hook
                                     #'org-appear-manual-stop
                                     nil
                                     t))))

(use-package org-autolist
  :straight t
  :after org
  :hook ((org-mode-hook . org-autolist-mode)))

;; TODO Load this via org-bable-do-load-languages
(use-package ob-http
  :straight t
  :after org)

;; TODO Set up keybindings
(use-package org-mac-link :straight t)
;; (use-package 'org-re-reveal :straight t)

;; TODO Figure out how to get the tree view to show up for this package
;;(use-package org-sidebar :straight t)

(use-package org-evil
  :straight t
  :after '(evil org))

;; (use-package ob-http :straight t)
;; (use-package ox-pandoc :straight t)
