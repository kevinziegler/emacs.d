(use-package org
  :hook ((org-mode . visual-line-mode))
  :config
  (setopt org-auto-align-tags nil
          org-babel-results-keyword "results"
          org-cycle-separator-lines 0
          org-edit-src-content-indentation 0
          org-ellipsis " ⋯"
          org-fold-catch-invisible-edits 'show-and-error
          org-fold-core-style 'overlays
          org-hidden-keywords '(title)
          org-fontify-quote-and-verse-blocks t
          org-hide-emphasis-markers t
          org-html-doctype "html5"
          org-html-html5-fancy t
          org-image-actual-width nil
          org-insert-heading-respect-content t
          org-list-allow-alphabetical t
          org-list-demote-modify-bullet '(("-" . "+") ("+" . "1.") ("1." . "-"))
          org-pretty-entities t
          org-special-ctrl-a/e t
          org-tags-column 0
          org-use-property-inheritance t)

  (defmacro kdz/follow-suffix-link (base-url)
    `(lambda (suffix) (browse-url (string-join (list ,base-url suffix) "/") )))

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


  ;; (advice-add 'org-mks
  ;;             :override #'org-mks-pretty)
  ;; (advice-add 'org-capture-select-template
  ;;             :override #'org-capture-select-template-prettier)

  (defvar gitlab-hosts-alist nil)
  (defvar jira-host nil)

  (org-link-set-parameters "gh" :follow (kdz/follow-suffix-link "https://github.com"))
  (org-link-set-parameters "gl" :follow (kdz/follow-suffix-link "https://gitlab.com"))

  (when (boundp gitlab-hosts-alist)
    (dolist (host gitlab-hosts-alist)
      (let ((name (car host))
            (url (cdr host)))
        (org-link-set-parameters (concat "gl-" name)
                                 :follow (kdz/follow-suffix-link url)))))

  (when (and (boundp 'hosted-gitlab-host) hosted-gitlab-host)
    (org-link-set-parameters "hgl"
                             :follow (kdz/follow-suffix-link hosted-gitlab-host)))

  (when (and (boundp 'jira-host) jira-host)
    (org-link-set-parameters "jira"
                             :follow (kdz/follow-suffix-link (format "%s/browse"
                                                                     jira-host)))))

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
  (setopt org-appear-trigger 'manual
          org-appear-autolinks t
          org-appear-autokeywords t)

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

;; TODO Load this via org-babel-do-load-languages
(use-package ob-http :straight t :after org)

;; TODO Set up keybindings
(use-package org-mac-link :straight t
  :config
  ;; (advice-add 'org-mac-link-firefox-insert-frontmost-url
  ;;             :around #'kdz/org-mac-link-advise-evil)
  ;; (advice-add 'org-mac-link-finder-insert-selected
  ;;             :around #'kdz/org-mac-link-advise-evil)
  )

(use-package org-re-reveal 
  :straight t
  :config
  (setq
   org-re-reveal-subtree-with-title-slide t
   org-re-reveal-transition "slide"
   org-re-reveal-title-slide "<h1>%t</h1><p>%a | %d</p>"
   org-re-reveal-plugins '(highlight markdown notes search zoom)))

;; TODO Figure out how to get the tree view to show up for this package
;; (use-package org-sidebar :straight t)

(use-package evil-org
  :straight t
  :after (evil org)
  :hook ((org-mode . evil-org-mode))
  :config
  (add-hook 'org-mode-hook
            (lambda () (add-hook 'evil-insert-state-exit-hook
                                 (lambda () (when (org-at-table-p)
                                              (org-cycle)))))))

(use-package ox
  :straight nil
  :config
  (defun kdz/ox-filter-git-file-link (data backend channel)
    "Transform file links in DATA into git-link URLs when appropriate."
    (let* ((beg (next-property-change 0 data))
           (link (if beg (get-text-property beg :parent data)))
           (type (org-element-property :type link))
           (path (org-element-property :path link))
           (option (org-element-property :search-option link)))
      (when (and (equal (org-element-property :type link) "file")
                 (eq (vc-backend path) 'Git))
        (format "[[%s][%s]]"
                (kdz/file-link-as-git-link path option)
                (org-element-contents link)))))

  (add-to-list 'org-export-filter-link-functions #'kdz/ox-filter-git-file-link))

;; TODO Figure out a good keybinding for this
(use-package ox-clip :straight t)

;; (use-package doct :straight t)
;; (use-package ob-http :straight t)
;; (use-package ob-mermaid :straight t)
;; (use-package org-jira :straight t)
;; (use-package valign :straight t)

(use-package ox-pandoc
  :straight t
  :config
  (add-to-list 'org-pandoc-options '(wrap . "none")))

(use-package ob-async
  :straight t
  :config

  ;; This addresses changes in the org-babel API that have broken ob-async:
  ;; https://github.com/astahlman/ob-async/issues/92
  ;;
  ;; First, we define a replacement to ob-async's execute function to
  ;; effectively apply this patch:
  ;;
  ;; https://github.com/astahlman/ob-async/pull/93/files
  ;;
  ;; This new version of the function is implemented as advice to override the
  ;; intitial function definition from ob-async.
  ;;
  ;; Then, we make the corresponding change to Doom's own helper advice and
  ;; replace the advice from the Doom org module with an additional
  ;; org-load-hook.
  ;;
  ;; Once the ob-async issue is merged and updated in Doom's module dependencies
  ;; the "override" advice can be removed.  Finally, once doom's own module code
  ;; is updated as well we should be able to remove this adivce entirely.
  (defun kdz/ob-async-org-babel-execute-src-block
      (&optional orig-fun arg info params &rest other-args)
    "Like org-babel-execute-src-block, but run asynchronously.

Original docstring for org-babel-execute-src-block:

Execute the current source code block.  Insert the results of
execution into the buffer.  Source code execution and the
collection and formatting of results can be controlled through a
variety of header arguments.

With prefix argument ARG, force re-execution even if an existing
result cached in the buffer would otherwise have been returned.

Optionally supply a value for INFO in the form returned by
`org-babel-get-src-block-info'.

Optionally supply a value for PARAMS which will be merged with
the header arguments specified at the front of the source code
block."
    (interactive "P")
    (cond
     ;; If this function is not called as advice, do nothing
     ((not orig-fun)
      (warn "ob-async-org-babel-execute-src-block is no longer needed in org-ctrl-c-ctrl-c-hook")
      nil)
     ;; If there is no :async parameter, call the original function
     ((not (assoc :async (nth 2 (or info (org-babel-get-src-block-info)))))
      (apply orig-fun arg info params other-args))
     ;; If the src block language is in the list of languages async is not to be
     ;; used for, call the original function
     ((member (nth 0 (or info (org-babel-get-src-block-info)))
              ob-async-no-async-languages-alist)
      (apply orig-fun arg info params other-args))
     ;; Otherwise, perform asynchronous execution
     (t
      (let ((placeholder (ob-async--generate-uuid)))
        ;; Here begins the original source of org-babel-execute-src-block
        (let* ((org-babel-current-src-block-location
                (or org-babel-current-src-block-location
                    (nth 5 info)
                    (org-babel-where-is-src-block-head)))
               (src-block-marker (save-excursion
                                   (goto-char org-babel-current-src-block-location)
                                   (point-marker)))
               (info (if info (copy-tree info) (org-babel-get-src-block-info))))
          ;; Merge PARAMS with INFO before considering source block
          ;; evaluation since both could disagree.
          (cl-callf org-babel-merge-params (nth 2 info) params)
          (when (org-babel-check-evaluate info)
            (cl-callf org-babel-process-params (nth 2 info))
            (let* ((params (nth 2 info))
                   (cache (let ((c (cdr (assq :cache params))))
                            (and (not arg) c (string= "yes" c))))
                   (new-hash (and cache (org-babel-sha1-hash info)))
                   (old-hash (and cache (org-babel-current-result-hash)))
                   (current-cache (and new-hash (equal new-hash old-hash)))
                   (result-params (cdr (assq :result-params params))))
              (cond
               (current-cache
                (save-excursion		;Return cached result.
                  (goto-char (org-babel-where-is-src-block-result nil info))
                  (forward-line)
                  (skip-chars-forward " \t")
                  (let ((result (org-babel-read-result)))
                    (message (replace-regexp-in-string "%" "%%" (format "%S" result)))
                    result)))
               ((org-babel-confirm-evaluate info)
                ;; Insert a GUID as a placeholder in our RESULTS block
                (when (not (or (member "none" result-params)
                               (member "silent" result-params)))
                  (org-babel-insert-result placeholder '("replace")))
                (let* ((lang (nth 0 info))
                       ;; Expand noweb references in BODY and remove any
                       ;; coderef.
                       (body
                        (let ((coderef (nth 6 info))
                              (expand
                               (if (org-babel-noweb-p params :eval)
                                   (org-babel-expand-noweb-references info)
                                 (nth 1 info))))
                          (if (not coderef) expand
                            (replace-regexp-in-string
                             (org-src-coderef-regexp coderef) "" expand nil nil 1))))
                       (dir (cdr (assq :dir params)))
                       (default-directory
                        (or (and dir (file-name-as-directory (expand-file-name dir)))
                            default-directory))
                       (cmd (intern (concat "org-babel-execute:" lang)))
                       (org-babel-async-content
                        (buffer-substring-no-properties (point-min) (point-max)))
                       result)
                  (unless (fboundp cmd)
                    (error "No org-babel-execute function for %s!" lang))
                  (message "executing %s code block%s..."
                           (capitalize lang)
                           (let ((name (nth 4 info)))
                             (if name (format " (%s)" name) "")))
                  (progn
                    (async-start
                     `(lambda ()
                        ;; TODO: Put this in a function so it can be overidden
                        ;; Initialize the new Emacs process with org-babel functions
                        (setq exec-path ',exec-path)
                        (setq load-path ',load-path)
                        ,(async-inject-variables ob-async-inject-variables)
                        (package-initialize)
                        (setq ob-async-pre-execute-src-block-hook ',ob-async-pre-execute-src-block-hook)
                        (run-hooks 'ob-async-pre-execute-src-block-hook)
                        (org-babel-do-load-languages 'org-babel-load-languages ',org-babel-load-languages)
                        (let ((default-directory ,default-directory))
                          (with-temp-buffer
                            (insert org-babel-async-content)
                            (,cmd ,body ',params))))
                     `(lambda (result)
                        (with-current-buffer ,(current-buffer)
                          (let ((default-directory ,default-directory))
                            (save-excursion
                              (cond
                               ((member "none" ',result-params)
                                (message "result silenced"))
                               ((member "silent" ',result-params)
                                (message (replace-regexp-in-string "%" "%%" (format "%S" result))))
                               (t
                                (goto-char ,src-block-marker)
                                (let ((file (cdr (assq :file ',params))))
                                  (when file
                                    ;; when result type is link, don't write result content to file.
                                    (unless (member "link" ',result-params)
                                      ;; If non-empty result and :file then write to :file.
                                      (when result
                                        (with-temp-file file
                                          (insert (org-babel-format-result
                                                   result (cdr (assq :sep ',params)))))))
                                    (setq result file))
                                  ;; Possibly perform post process provided its
                                  ;; appropriate.  Dynamically bind "*this*" to the
                                  ;; actual results of the block.
                                  (let ((post (cdr (assq :post ',params))))
                                    (when post
                                      (let ((*this* (if (not file) result
                                                      (org-babel-result-to-file
                                                       file
                                                       (let ((desc (assq :file-desc ',params)))
                                                         (and desc (or (cdr desc) result)))))))
                                        (setq result (org-babel-ref-resolve post))
                                        (when file
                                          (setq result-params (remove "file" ',result-params))))))
                                  (org-babel-insert-result result ',result-params ',info ',new-hash ',lang))))
                              (run-hooks 'org-babel-after-execute-hook)))))))))))))))))

  (defun kdz/+org-babel-disable-async-maybe-a
      (fn &optional orig-fn arg info params &rest other-args)
    "Use ob-comint where supported, disable async altogether where it isn't.

We have access to two async backends: ob-comint or ob-async, which have
different requirements. This advice tries to pick the best option between them,
falling back to synchronous execution otherwise. Without this advice, they die
with an error; terrible UX!

Note: ob-comint support will only kick in for languages listed in
`+org-babel-native-async-langs'.

Also adds support for a `:sync' parameter to override `:async'."
    (if (null orig-fn)
        (apply fn orig-fn arg info params other-args)
      (let* ((info (or info (org-babel-get-src-block-info)))
             (params (org-babel-merge-params (nth 2 info) params)))
        (if (or (assq :sync params)
                (not (assq :async params))
                (member (car info) ob-async-no-async-languages-alist)
                ;; ob-comint requires a :session, ob-async does not, so fall
                ;; back to ob-async if no :session is provided.
                (unless (member (alist-get :session params) '("none" nil))
                  (unless (memq (let* ((lang (nth 0 info))
                                       (lang (cond ((symbolp lang) lang)
                                                   ((stringp lang) (intern lang)))))
                                  (or (alist-get lang +org-babel-mode-alist)
                                      lang))
                                +org-babel-native-async-langs)
                    (message "Org babel: %s :session is incompatible with :async. Executing synchronously!"
                             (car info))
                    (sleep-for 0.2)) t))
            (apply orig-fn arg info params other-args)
          (apply fn orig-fn arg info params other-args)))))

  (advice-add 'ob-async-org-babel-execute-src-block
              :override
              #'kdz/ob-async-org-babel-execute-src-block)
  (advice-add 'ob-async-org-babel-execute-src-block
              :around
              #'kdz/+org-babel-disable-async-maybe-a))

(use-package org-make-toc
  :straight t
  :config
  (defvar kdz-org-make-toc-headline "Table of Contents")
  (defun kdz/org-make-toc-dwim ()
    (interactive)
    (let ((toc-point (org-find-exact-headline-in-buffer kdz-org-make-toc-headline)))
      (if toc-point
          (save-excursion
            (goto-char toc-point)
            (when (not (org-entry-get (point) "TOC"))
              (end-of-line)
              (newline)
              (org-make-toc-insert))
            (org-make-toc))
        (save-excursion
          (goto-char (point-min))
          (org-next-visible-heading (point-min))
          (previous-line)
          (org-insert-heading)
          (insert kdz-org-make-toc-headline)
          (kdz/org-make-toc-init))))))
