(use-package consult
  :straight t
  :config
  (setq consult-narrow-key "<")

  (defun kdz/consult-ripgrep-selected (start end)
    (interactive "r")
    (let ((initial (when (use-region-p)
                     (buffer-substring start end))))
      (consult-ripgrep nil initial))))

(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)             ;; pick some comfortable binding
   ("C-;" . embark-dwim)            ;; good alternative: M-.
   ("C-h B" . embark-bindings)      ;; alternative for `describe-bindings'
   ("s-<return>" . embark-collect)) ;; 90% of what I want to do is persist results

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
  The which-key help message will show the type and value of the
  current target followed by an ellipsis if there are further
  targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :straight t
  :hook ((embark-collect-mode . consult-preview-at-point-mode))
  :config
  (defvar kdz-search-query-rx
    "^\\*Embark Collect: consult-ripgrep - #\\(.+\\)\\*$"
    "Regexp to extract a search query from a buffer name")

  (defun kdz/embark-collect-ripgrep-pair (buffer)
    (let* ((name (buffer-name buffer)))
      (cons (save-match-data
              (string-match kdz-search-query-rx name)
              (match-string 1 name))
            buffer)))

  (defun kdz/consult-embark-ripgrep-collected ()
    "Provide buffers for collected ripgrep results as a consult source"
    (consult--buffer-query :sort 'visibility
                           :as #'kdz/embark-collect-ripgrep-pair
                           :include "^\*Embark Collect: consult-ripgrep"))

  (defun kdz/annotate-ripgrep-results (buffer)
    "Marginalia annotation for collected search results"
    (let* ((count-results (with-current-buffer buffer
                            (how-many "^[0-9]+:" (point-min) (point-max))))
           (count-files (- (car (buffer-line-statistics buffer))
                           count-results)))

      (marginalia--fields
       (count-results :format "[%d lines]" :face 'marginalia-number)
       (count-files :format "[%d files]" :face 'marginalia-number))))

  (defvar consult--source-embark-collect-ripgrep
    (list :name "Search Results [rg]"
          :category 'embark-collect-ripgrep
          :narrow ?r
          :face 'consult-buffer
          :history 'embark-collect-ripgrep
          :state #'consult--buffer-state
          :items #'kdz/consult-embark-ripgrep-collected)
    "Consult source specifically for embark-ripgrep results")

  (defun kdz/consult-embark-ripgrep-results ()
    "Show a previously collected set of results from `consult-ripgrep'"
    (interactive)
    (consult-buffer '(consult--source-embark-collect-ripgrep)))

  (add-to-list 'marginalia-annotator-registry
               '(embark-collect-ripgrep kdz/annotate-ripgrep-results)))

(use-package marginalia :straight t
  :init (marginalia-mode)
  :config
  (setq marginalia-align 'right))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :straight t
  :init
  (vertico-mode)

  (defun kdz/vertico--format-candiate-marker-advice
      (orig cand prefix suffix index start)
    (setq cand (funcall orig cand prefix suffix index start))
    (concat (if (= vertico--index index)
                (propertize "» " 'face 'vertico-current)
              "  ")
            cand))

  (advice-add #'vertico--format-candidate
              :around #'kdz/vertico--format-candiate-marker-advice))

(use-package vertico-directory
  :after vertico
  :straight nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy)))

(use-package consult-todo :straight t)

(provide 'packages.d/tools/minibuffer)
