(use-package org
  :ensure (:wait t)
  :hook ((org-mode . visual-line-mode)
         (org-mode . visual-wrap-prefix-mode)
         (org-mode . kdz/org-mode-set-electric-pair-predicate))
  :general
  (general-def
    :states 'insert
    :keymaps 'org-mode-map
    "s-RET" 'kdz/org-return-dwim)

  (kdz/leader-file-def "l" '("Store Link" . org-store-link))

  :general-config
  (kdz/mode-leader-def
    :keymaps 'org-mode-map
    "e"   (cons "Edit" (make-sparse-keymap))
    "eb" '("Edit Org Block" . org-edit-special)
    "et" '("Update Title"   . kdz/org-rename-title)

    "i"   (cons "Insert" (make-sparse-keymap))
    "ic" '("Insert Table of Contents"       . kdz/org-make-toc-dwim)
    "if" '("Insert Footnote (inline)"       . org-footnote-new)
    "iF" '("Insert Footnote (outline)"      . kdz/org-insert-outline-footnote)
    "ih" '("Insert Heading"                 . org-insert-heading-respect-content)
    "iH" '("Insert Heading (one level up)"  . kdz/org-insert-heading-one-level-up)
    "is" '("Insert Sub-heading"             . org-insert-subheading)
    "il" '("Insert Link (From Application)" . org-mac-link-get-link)

    "j"   (cons "Jump" (make-sparse-keymap))
    "jt" '("Top-Level of current sub-tree")

    "n"   (cons "Narrow" (make-sparse-keymap))
    "nn" '("Narrow to Sub-tree"             . org-narrow-to-subtree)
    "ne" '("Narrow to current element"      . org-narrow-to-element)
    "nb" '("Narrow to current block"        . org-narrow-to-block)

    "s"   (cons "Scheduling" (make-sparse-keymap))
    "ss" '("Schedule Entry"                 . org-schedule)
    "sd" '("Set Deadline for Entry"         . org-deadline)

    "t"   (cons "Toggle" (make-sparse-keymap))
    "tn" '("Numbered Headings"              . org-num-mode)

    "x"  '("Export Document"                . org-export-dispatch))

  :config
  (require 'lib/org-spacing)
  (setopt org-auto-align-tags nil
          org-babel-results-keyword "results"
          org-cycle-separator-lines 0
          org-edit-src-content-indentation 0
          org-ellipsis " ⋯"
          org-fold-catch-invisible-edits 'show-and-error
          org-fold-core-style 'overlays
          org-footnote-section nil
          org-fontify-quote-and-verse-blocks t
          org-hidden-keywords '(title)
          org-hide-emphasis-markers t
          org-html-doctype "html5"
          org-html-html5-fancy t
          org-image-actual-width nil
          org-insert-heading-respect-content t
          org-list-allow-alphabetical t
          org-list-demote-modify-bullet '(("-" . "+") ("+" . "1.") ("1." . "-"))
          org-log-done 'time
          org-log-into-drawer t
          org-M-RET-may-split-line '((default . nil))
          org-pretty-entities t
          org-return-follows-link t
          org-special-ctrl-a/e t
          org-tags-column 0
          org-use-property-inheritance t)

  (add-to-list 'display-buffer-alist
               '(kdz/notes-file-p display-buffer-in-tab (tab-name . "Notes")))

  (defun kdz/notes-file-p (buffer &rest _)
    "Determine if a file should be consider a 'notebook' item"
    (s-starts-with? (expand-file-name org-directory)
                    (buffer-file-name (get-buffer buffer))))

  (defun kdz/notes ()
    "Open a file in `kdz-notes-dir'"
    ;; TODO Figure out why the prefix arg isn't causing `find-file' to ignore
    ;; `display-buffer-alist'
    (interactive)
    (let ((default-directory (if (s-ends-with-p "/" org-directory)
                                 org-directory
                               (concat org-directory "/"))))
      (call-interactively #'find-file)))

  (defun kdz/org-insert-outline-footnote ()
    (interactive)
    (let ((org-footnote-section "Footnotes"))
      (funcall-interactively 'org-footnote-new)))

  (defun kdz/org-insert-heading-one-level-up ()
    (interactive)
    (org-insert-heading nil nil (- (org-current-level) 1)))

  (defun kdz/org-output-dir ()
    "Helper to set the default path for org babel outputs (via the :dir header)"
    (concat (file-name-base (buffer-file-name)) ".outputs"))

  (defun kdz/org-export-path (file-name &optional subpath)
    (interactive "f")
    (concat (file-name-base file-name)
            ".exports"
            (when subpath "/")
            subpath))

  (defun kdz/org-goto-top-level ()
    (interactive)
    (let ((org-goto-interface 'outline-path-completion)
          (org-goto-max-level 1))
      (org-goto)
      (org-narrow-to-subtree)))

  (defun stolen/get-keyword-key-value (kwd)
    (let ((data (cadr kwd)))
      (list (plist-get data :key)
            (plist-get data :value))))

  (defun stolen/org-current-buffer-get-title ()
    "Get the title of the current org-mode buffer"
    (nth 1
         (assoc "TITLE"
                (org-element-map (org-element-parse-buffer 'greater-element)
                    '(keyword)
                  #'stolen/get-keyword-key-value))))

  (defun kdz/string-to-filename (name-string word-sep extension)
    "Sanitize a string for use as a filename"
    (let* ((name-alphanumeric
            (replace-regexp-in-string "[^A-Za-z0-9 ]" "" name-string))
           (basename (string-replace " " word-sep (downcase name-alphanumeric))))
      (file-name-with-extension basename extension)))

  (defun kdz/org-sync-title-to-filename ()
    "Sync the title of the current org document with the filename"
    (let* ((title (stolen/org-current-buffer-get-title))
           (basename (kdz/string-to-filename
                      title "-" (file-name-extension (buffer-file-name)))))
      (doom/move-this-file (expand-file-name
                            basename (file-name-directory (buffer-file-name))))))

  (defun kdz/org-update-title (new-title)
    "Update the title of an org-mode document, or add one if none exists"
    (save-excursion
      (goto-char (point-min))
      (condition-case nil
          (progn (search-forward "#+title: ")
                 (kill-line)
                 (insert new-title))
        ('error (insert (format "#+title: %s\n" new-title))))))

  (defun kdz/org-rename-title ()
    "Update the title of an org-mode document interactively, and rename the file"
    (interactive)
    (let ((new-title (read-string (format "Re-Title \"%s\" to: "
                                          (stolen/org-current-buffer-get-title)))))
      (kdz/org-update-title new-title)
      (kdz/org-sync-title-to-filename)))

  (defmacro kdz/follow-suffix-link (base-url)
    `(lambda (suffix) (browse-url (string-join (list ,base-url suffix) "/") )))

  (defun unpackaged/org-element-descendant-of (type element)
    "Return non-nil if ELEMENT is a descendant of TYPE.
TYPE should be an element type, like `item' or `paragraph'.
ELEMENT should be a list like that returned by `org-element-context'."
    ;; MAYBE: Use `org-element-lineage'.
    (when-let* ((parent (org-element-property :parent element)))
      (or (eq type (car parent))
          (unpackaged/org-element-descendant-of type parent))))

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
          (t (progn
               (let ((at-first-table-line (save-excursion
                                            (previous-line)
                                            (beginning-of-line)
                                            (not (org-at-table-p)))))
                 (if at-first-table-line
                     (progn
                       (org-return t)
                       (beginning-of-line)
                       (forward-char)
                       (insert "-")))
                 (org-return t))))))

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

  (defun kdz/org-mode-set-electric-pair-predicate ()
    (add-function :before-until
                  (local 'electric-pair-inhibit-predicate)
                  (lambda (c) (eq c ?<))))

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
  :ensure nil
  :after org
  :custom
  (org-agenda-tags-column 0)
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid '((daily today require-timed)
                          (800 1000 1200 1400 1600 1800 2000)
                          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────"))

(use-package org-modern
  :after (org nerd-icons)
  :hook (elpaca-after-init . global-org-modern-mode)
  :custom
  (org-modern-star 'replace)
  (org-modern-checkbox `((?X  . ,(nerd-icons-mdicon "nf-md-checkbox_marked"))
                         (?-  . ,(nerd-icons-mdicon "nf-md-checkbox_intermediate"))
                         (?\s . ,(nerd-icons-mdicon "nf-md-checkbox_blank_outline")))))

(use-package org-appear
  :after org
  :hook ((org-mode . org-appear-mode)
         (org-mode . kdz/org-appear-respect-evil-state))
  :custom
  (org-appear-trigger 'manual)
  (org-appear-autolinks t)
  (org-appear-autokeywords t)
  (org-appear-manual-linger t)
  :config
  (defun kdz/org-appear-respect-evil-state ()
    (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
    (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t)))

(use-package org-autolist :after org :hook ((org-mode . org-autolist-mode)))

;; TODO Load this via org-babel-do-load-languages
(use-package ob-http :after org)

;; TODO Set up keybindings
(use-package org-mac-link
  :config
  (defun kdz/org-mac-link-advise-evil (org-mac-link-fn &rest orig-args)
    "Advice to org-mac-link functions to handle insertion with evil-mode"
    (interactive)
    (let ((char-at-insert (thing-at-point 'char)))
      (evil-save-state
        (evil-append 1)
        (when (not (string-match-p "[[:space:]]" char-at-insert)) (insert " "))
        (apply org-mac-link-fn orig-args))))

  (advice-add 'org-mac-link-firefox-insert-frontmost-url
              :around #'kdz/org-mac-link-advise-evil)
  (advice-add 'org-mac-link-finder-insert-selected
              :around #'kdz/org-mac-link-advise-evil))

(use-package org-re-reveal
  :custom
  (org-re-reveal-subtree-with-title-slide t)
  (org-re-reveal-transition "slide")
  (org-re-reveal-title-slide "<h1>%t</h1><p>%a | %d</p>")
  (org-re-reveal-plugins '(highlight markdown notes search zoom)))

(use-package evil-org
  :after (evil org)
  :hook ((org-mode . evil-org-mode)
         (org-mode . kdz/org-cycle-table-on-evil-state))
  :general
  (general-def :states 'normal :keymaps 'org-mode-map "RET" 'evil-org-return)

  :config
  (evil-org-set-key-theme
   '(textobjects insert navigation additional shift todo heading))

  (defun kdz/org-cycle-table-on-evil-state ()
    (add-hook 'evil-insert-state-exit-hook
              (lambda () (when (org-at-table-p) (org-cycle))))))

(use-package ox
  :ensure nil
  :config
  (defun kdz/line-number-from-fragment (fragment)
    "Find line number of FRAGMENT in current buffer"
    (when fragment
      (save-excursion (goto-char (point-min))
                      (search-forward fragment)
                      (line-number-at-pos))))

  (defun kdz/file-link-as-git-link (git-file option)
    "Convert a file path link to a git-controlled file to a git-link URL"
    (let* ((existing-buffer (get-file-buffer git-file))
           (file-buffer (or existing-buffer (find-file-noselect git-file)))
           (option-as-number (when (and (stringp option)
                                        (string-match-p "\\`[0-9]+\\'" option))
                               (string-to-number option)))
           (url
            (with-current-buffer file-buffer
              ;; `git-link'  returns nil on success or a string on error
              (unless (git-link (git-link--remote)
                                (or option-as-number
                                    (kdz/line-number-from-fragment option))
                                nil)
                (car kill-ring)))))
      (unless existing-buffer (kill-buffer file-buffer))
      url))

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
(use-package ox-clip)

;; (use-package doct)
;; (use-package ob-http)
;; (use-package ob-mermaid)
;; (use-package org-jira)
;; (use-package valign)

(use-package ox-pandoc
  :if (executable-find "pandoc")
  :config (add-to-list 'org-pandoc-options '(wrap . "none")))

(use-package ob-plantuml
  :ensure nil
  :custom
  (org-plantuml-exec-mode 'executable)
  (org-plantuml-exec-mode 'plantuml)
  :config
  (defun kdz/org-babel-plantuml-format-var (var-value)
    (cond ((numberp var-value) (number-to-string var-value))
          ((booleanp var-value) (if var-value "true" "false"))
          (t (format "\"%s\"" var-value))))

  (defun kdz/org-babel-variable-assignments:plantuml (params)
    "Return a list of PlantUML statements assigning the block's variables.

PARAMS is a property list of source block parameters, which may
contain multiple entries for the key `:var'.  `:var' entries in PARAMS
are expected to be scalar variables."
    (mapcar
     (lambda (pair)
       (format "!$%s=%s"
               (car pair)
               (kdz/org-babel-plantuml-format-var (cdr pair))))
     (org-babel--get-vars params)))

  (defun kdz/ob-plantuml-directive-theme (params)
    "Process :theme and :theme-from header args into an ob-plantuml block"
    (let ((theme (cdr (assq :theme params)))
          (theme-from (cdr (assq :theme-from params))))
      (cond ((and theme theme-from) (format "!theme %s from %s"
                                            theme
                                            (expand-file-name theme-from)))
            (theme (format "!theme %s" theme))
            (t ""))))

  (defun kdz/ob-plantuml-wrap-body (type &rest body-parts)
    "Wrap BODY-PARTS with plantuml @start/@end anchors for TYPE"
    (string-join
     (append (list (concat "@start" type))
             body-parts
             (list (concat "@end" type)))
     "\n"))

  (defun kdz/org-babel-plantuml-make-body (body params)
    "Pre-process ob-plantuml BODY with PARAMS

This extends the default ob-plantuml behavior to support new header args:
 - :type :: The type of diagram to apply @start/@end anchors
 - :theme :: The plantuml theme to use for the diagram
 - :theme-from :: The path to find the theme specified by :theme

Additionally, :var VAR=VAL header args are processed at $!VAR=VAL instead of
using the !define VAR VAL syntax"
    (let ((theme-directive (kdz/ob-plantuml-directive-theme params))
          (type (or (cdr (assq :type params)) "uml"))
          (full-body
           (org-babel-expand-body:generic
            body params (org-babel-variable-assignments:plantuml params))))
      (if (string-prefix-p "@start" body t) full-body
        (kdz/ob-plantuml-wrap-body type theme-directive full-body))))

  (advice-add 'org-babel-variable-assignments:plantuml
              :override #'kdz/org-babel-variable-assignments:plantuml)
  (advice-add 'org-babel-plantuml-make-body
              :override #'kdz/org-babel-plantuml-make-body))

(use-package ob-async
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

(use-package org-sliced-images
  :config
  (defalias 'org-remove-inline-images  #'org-sliced-images-remove-inline-images)
  (defalias 'org-toggle-inline-images  #'org-sliced-images-toggle-inline-images)
  (defalias 'org-display-inline-images #'org-sliced-images-display-inline-images))

(use-package org-src :ensure nil :custom (org-src-preserve-indentation t))

(use-package org-tidy
  :general
  (kdz/mode-leader-def
    :keymaps 'org-mode-map
    "tt" '("Toggle Org Tidy" . org-tidy-toggle))
  :hook (org-mode . org-tidy-mode))

(use-package org-ql
  :config
  (org-ql-defpred shopping-list (store)
    "Collect items to build a shopping list for STORE"
    :body (and (outline-path store) (regexp "- \\[ \\]"))))

(use-package om-dash
  :ensure (om-dash :host github
                   :repo "gavv/om-dash"
                   :branch "main"
                   :files ("om-dash.el")))


(use-package org-chef
  :after org
  :general
  (kdz/mode-leader-def
    :keymaps 'org-mode-map
    "iR" '("Insert Recipe (By URL)" . org-chef-insert-recipe)))

(use-package org-ac :config (org-ac/config-default))

(provide 'packages.d/org)
