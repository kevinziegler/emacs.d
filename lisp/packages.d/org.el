(use-package org
  :hook ((org-mode . visual-line-mode))
  :config
  (setopt org-auto-align-tags nil
          org-tags-column 0
          org-fold-catch-invisible-edits 'show-and-error
          org-special-ctrl-a/e t
          org-insert-heading-respect-content t
          ;; Org styling, hide markup etc.
          org-hide-emphasis-markers t
          org-pretty-entities t
          org-ellipsis " …"
          org-cycle-separator-lines 0
          org-fold-core-style 'overlays
          org-edit-src-content-indentation 0)

  (defun kdz/org-return-handle-point-at-heading ()
    "Handle <return> behavior when point is at an org heading"
    (let ((heading-start (org-entry-beginning-position)))
      (goto-char (org-entry-end-position))
      (cond ((and (org-at-heading-p)
                  (= heading-start (org-entry-beginning-position)))
             ;; Entry ends on its heading; add newline after
             (end-of-line)
             (insert "\n\n"))
            (t
             ;; Entry ends after its heading; back up
             (forward-line -1)
             (end-of-line)
             (when (org-at-heading-p)
               ;; At the same heading
               (forward-line)
               (insert "\n")
               (forward-line -1))
             (while (not (looking-back "\\(?:[[:blank:]]?\n\\)\\{3\\}" nil))
               (insert "\n"))
             (forward-line -1)))))

  (defun kdz/org-return-handle-point-at-checkbox ()
    "Handle <return> behavior when point is at a checkbox item"
    (if (string-match-p "^[[:space:]]*[-+] \\[ \\][[:space:]]*$"
                        (thing-at-point 'line))
        ;; Last list item was empty; remove it and insert an empty line
        (progn
          (beginning-of-line)
          (kill-line)
          (insert "\n"))
      (org-insert-todo-heading nil)))

  (defun kdz/org-return-handle-point-in-item ()
    "Handle <return> behavior when point is inside a list item"
    (let ((context (org-element-context)))
      (if (or (eq 'plain-list (car context))  ; First item in list
              (and (eq 'item (car context))
                   (not (eq (org-element-property :contents-begin context)
                            (org-element-property :contents-end context))))
              (unpackaged/org-element-descendant-of 'item context))  ; Element in list item, e.g. a link
          ;; Non-empty item: Add new item.
          (org-insert-item)
        ;; Empty item: Close the list.
        ;; TODO: Do this with org functions rather than operating on
        ;;       the text. Can't seem to find the right function.
        (delete-region (line-beginning-position) (line-end-position))
        (insert "\n"))))

  (defun kdz/org-return-handle-point-inside-table ()
    "Handle <return> behavior when point is inside a table"
    (cond ((save-excursion
             (beginning-of-line)
             ;; See `org-table-next-field'.
             (cl-loop with end = (line-end-position)
                      for cell = (org-element-table-cell-parser)
                      always (equal (org-element-property :contents-begin cell)
                                    (org-element-property :contents-end cell))
                      while (re-search-forward "|" end t)))
           ;; Empty row: end the table.
           (delete-region (line-beginning-position) (line-end-position))
           (org-return t))
          ;; Non-empty row: call `org-return-indent'.
          (t (org-return t))))

;;;###autoload
  (defun kdz/org-return-dwim (&optional default)
    "A helpful replacement for `org-return-indent'.  With prefix, call `org-return-indent'.

On headings, move point to position after entry content.  In
lists, insert a new item or end the list, with checkbox if
appropriate.  In tables, insert a new row or end the table."
    ;; Lifted and modified from Sacha Chua's Emacs configuration:
    ;; https://sachachua.com/dotemacs/index.html
    ;; Inspired by John Kitchin: http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/

    (interactive "P")
    (if default
        (org-return t)
      (cond
       ((org-at-heading-p) (kdz/org-return-handle-point-at-heading))
       ((org-at-item-checkbox-p) (kdz/org-return-handle-point-at-checkbox))
       ((org-in-item-p) (kdz/org-return-handle-point-in-item))

       ((when (fboundp 'org-inlinetask-in-task-p)
          (org-inlinetask-in-task-p))
        ;; Inline task: Don't insert a new heading.
        (org-return t))
       ((org-at-table-p) (kdz/org-return-handle-point-inside-table))
       ;; All other cases: call `org-return-indent'.
       (t (org-return t)))))

  (defun kdz/org-heading-fixup-new-line ()
    "Ensure an empty line between non-empty org-mode headings"
    (when (org-at-heading-p)
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
     (emacs-lisp .t)))

  ;; (advice-add 'org-babel-variable-assignments:plantuml
  ;;             :override #'kdz/org-babel-variable-assignments:plantuml)
  ;; (advice-add 'org-babel-plantuml-make-body
  ;;             :override #'kdz/org-babel-plantuml-make-body)
  ;; (advice-add 'org-mks
  ;;             :override #'org-mks-pretty)
  ;; (advice-add 'org-capture-select-template
  ;;             :override #'org-capture-select-template-prettier)

  )

(use-package org-agenda
  :straight nil
  :after org
  :config
  (setopt org-agenda-tags-column 0
          org-agenda-block-separator ?─
          org-agenda-time-grid '((daily today require-timed)
                                 (800 1000 1200 1400 1600 1800 2000)
                                 " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
          org-agenda-current-time-string
          "⭠ now ─────────────────────────────────────────────────"))

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
(use-package ob-http :straight t :after org)

;; TODO Set up keybindings
(use-package org-mac-link :straight t
  :config
  ;; (advice-add 'org-mac-link-firefox-insert-frontmost-url
  ;;             :around #'kdz/org-mac-link-advise-evil)
  ;; (advice-add 'org-mac-link-finder-insert-selected
  ;;             :around #'kdz/org-mac-link-advise-evil)
  )
;; (use-package 'org-re-reveal :straight t)

;; TODO Figure out how to get the tree view to show up for this package
;; (use-package org-sidebar :straight t)

(use-package evil-org
  :straight t
  :after (evil org)
  :hook ((org-mode . evil-org-mode)))

;; (use-package ox-pandoc :straight t)
