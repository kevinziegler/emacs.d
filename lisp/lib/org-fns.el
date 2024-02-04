(defun kdz/line-number-from-fragment (fragment)
  "Find line number of FRAGMENT in current buffer"
  (when fragment
    (save-excursion (goto-char (point-min))
                    (search-forward fragment)
                    (line-number-at-pos))))

(defun kdz/git-link--tag ()
  "Get the latest tag for constructing a git-link URL."
  (car (git-link--exec "describe" "--tags" "HEAD")))

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

(defun kdz/org-goto-top-level ()
  (interactive)
  (let ((org-goto-interface 'outline-path-completion)
        (org-goto-max-level 1))
    (org-goto)
    (org-narrow-to-subtree)))

(defun kdz/org-open-one-on-one ()
  (interactive)
  (find-file "~/notes/one-on-one-discussions.org")
  (when (buffer-narrowed-p) (widen))
  (let ((org-goto-interface 'outline-path-completion)
        (org-goto-max-level 1))
    (org-goto))
  (org-narrow-to-subtree)
  (org-show-todo-tree nil))

(defmacro kdz/org-appear-hook-evil-state (evil-state)
  `(lambda ()
     (add-hook ',(intern (concat "evil-" (symbol-name evil-state) "-entry-hook"))
               #'org-appear-manual-start nil t)
     (add-hook ',(intern (concat "evil-" (symbol-name evil-state) "-exit-hook"))
               #'org-appear-manual-stop nil t)))

(defun kdz/org-output-dir ()
  "Helper to set the default path for org babel outputs (via the :dir header)"
  (concat (file-name-base (buffer-file-name)) ".outputs"))

;; This works to insert a new line in cases where it's missing, sort of:
;; - It still doesn't address cases where the section element has _multiple_ newlines
;; - Need to see how it works when a section contains only newline(s)
;;
;; Rules for end-of-section blanks:
;; - If a section has content, then that section should end with a blank
;;   - "properties" an other metadata are not counted as content
;; - If the next section in the document is at a higher level (i.e. is not a
;;   sibling or a child), then the section should close with two blank lines
(defun kdz/section-add-newline-maybe (section-element edit-count)
  (when (eq 0 (org-element-property :post-blank section-element))
    (save-excursion
      (goto-char (+ edit-count (org-element-property :end section-element)))
      (cl-incf edit-count)
      (insert hard-newline))))

(defun kdz/org-inject-section-blanks ()
  (let ((edited-section-count 0))
    (org-element-map (org-element-parse-buffer) 'section
      (lambda (section-element)
        (kdz/section-add-newline-maybe section-element edited-section-count)))))

(defun kdz/org-export-path (file-name &optional subpath)
  (interactive "f")
  (concat (file-name-base file-name)
          ".exports"
          (when subpath "/")
          subpath))

(defun kdz/org-mac-link-advise-evil (org-mac-link-fn &rest orig-args)
  "Advice to org-mac-link functions to handle insertion with evil-mode"
  (interactive)
  (let ((char-at-insert (thing-at-point 'char)))
    (evil-save-state
      (evil-append 1)
      (when (not (string-match-p "[[:space:]]" char-at-insert))
        (insert " "))
      (apply org-mac-link-fn orig-args))))

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
org-capture-select-template-prettier (&optional keys)
"Select a capture template, in a prettier way than default
Lisp programs can force the template by setting KEYS to a string."
(let ((org-capture-templates
       (or (org-contextualize-keys
            (org-capture-upgrade-templates org-capture-templates)
            org-capture-templates-contexts)
           '(("t" "Task" entry (file+headline "" "Tasks")
              "* TODO %?\n  %u\n  %a")))))
  (if keys
      (or (assoc keys org-capture-templates)
          (error "No capture template referred to by \"%s\" keys" keys))
    (org-mks org-capture-templates
             "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
             "Template key: "
             `(("q" ,(concat (all-the-icons-octicon "stop"
                                                    :face 'all-the-icons-red
                                                    :v-adjust 0.01)
                             "\tAbort")))))))

(defun org-mks-pretty (table title &optional prompt specials)

  (save-window-excursion
    (let ((inhibit-quit t)
          (buffer (org-switch-to-buffer-other-window "*Org Select*"))
          (prompt (or prompt "Select: "))
          case-fold-search
          current)
      (unwind-protect
          (catch 'exit
            (while t
              (setq-local evil-normal-state-cursor (list nil))
              (erase-buffer)
              (insert title "\n\n")
              (let ((des-keys nil)
                    (allowed-keys '("\C-g"))
                    (tab-alternatives '("\s" "\t" "\r"))
                    (cursor-type nil))
                ;; Populate allowed keys and descriptions keys
                ;; available with CURRENT selector.
                (let ((re (format "\\`%s\\(.\\)\\'"
                                  (if current (regexp-quote current) "")))
                      (prefix (if current (concat current " ") "")))
                  (dolist (entry table)
                    (pcase entry
                      ;; Description.
                      (`(,(and key (pred (string-match re))) ,desc)
                       (let ((k (match-string 1 key)))
                         (push k des-keys)
                         ;; Keys ending in tab, space or RET are equivalent.
                         (if (member k tab-alternatives)
                             (push "\t" allowed-keys)
                           (push k allowed-keys))
                         (insert (propertize prefix 'face 'font-lock-comment-face)
                                 (propertize k 'face 'bold)
                                 (propertize "›" 'face 'font-lock-comment-face)
                                 "  "
                                 desc
                                 "…"
                                 "\n")))
                      ;; Usable entry.
                      (`(,(and key (pred (string-match re))) ,desc . ,_)
                       (let ((k (match-string 1 key)))
                         (insert (propertize prefix 'face 'font-lock-comment-face)
                                 (propertize k 'face 'bold)
                                 "   "
                                 desc
                                 "\n")
                         (push k allowed-keys)))
                      (_ nil))))
                ;; Insert special entries, if any.
                (when specials
                  (insert "─────────────────────────\n")
                  (pcase-dolist (`(,key ,description) specials)
                    (insert (format "%s   %s\n"
                                    (propertize key 'face '(bold all-the-icons-red))
                                    description))
                    (push key allowed-keys)))
                ;; Display UI and let user select an entry or
                ;; a sub-level prefix.
                (goto-char (point-min))
                (unless (pos-visible-in-window-p (point-max))
                  (org-fit-window-to-buffer))
                (let ((pressed (org--mks-read-key allowed-keys prompt nil)))
                  (setq current (concat current pressed))
                  (cond
                   ((equal pressed "\C-g") (user-error "Abort"))
                   ((equal pressed "ESC") (user-error "Abort"))
                   ;; Selection is a prefix: open a new menu.
                   ((member pressed des-keys))
                   ;; Selection matches an association: return it.
                   ((let ((entry (assoc current table)))
                      (and entry (throw 'exit entry))))
                   ;; Selection matches a special entry: return the
                   ;; selection prefix.
                   ((assoc current specials) (throw 'exit current))
                   (t (error "No entry available")))))))
        (when buffer (kill-buffer buffer))))))

(defun +doct-icon-declaration-to-icon (declaration)
  "Convert :icon declaration to icon"
  (let ((name (pop declaration))
        (set  (intern (concat "all-the-icons-" (plist-get declaration :set))))
        (face (intern (concat "all-the-icons-" (plist-get declaration :color))))
        (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
    (apply set `(,name :face ,face :v-adjust ,v-adjust))))

(defun +doct-iconify-capture-templates (groups)
  "Add declaration's :icon to each template group in GROUPS."
  (let ((templates (doct-flatten-lists-in groups)))
    (setq doct-templates (mapcar (lambda (template)
                                   (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                               (spec (plist-get (plist-get props :doct) :icon)))
                                     (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                                    "\t"
                                                                    (nth 1 template))))
                                   template)
                                 templates))))

(defun kdz/org-capture-no-modeline ()
  "Disable modeline when launching org-capture"
  (interactive)
  (set-window-parameter nil 'mode-line-format 'none)
  (org-capture))
