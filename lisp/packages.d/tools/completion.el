(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package company)

(use-package corfu
  :custom
  (corfu-auto t)
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode)))

(use-package nerd-icons-corfu
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package hotfuzz
  :config
  (add-to-list 'completion-styles 'hotfuzz))

(use-package consult
  :general
  (kdz/leader-search-def
    "*" '("Search for selection"              . kdz/consult-ripgrep-selected)
    "l" '("Search for Line (Current Buffer)"  . consult-line)
    "L" '("Search for Line (Project Buffers)" . consult-line-multi)
    "p" '("Project"                           . consult-ripgrep)
    "s" '("Thing-at-point (DWIM)"             . tap/consult-ripgrep-dwim)
    "S" '("Thing-at-point (Select)"           . tap/consult-ripgrep))
  :config
  (setq consult-narrow-key "<")

  (defun kdz/consult-ripgrep-selected (start end)
    (interactive "r")
    (let ((initial (when (use-region-p)
                     (buffer-substring start end))))
      (consult-ripgrep nil initial))))

(use-package consult-yasnippet
  :after (consult yasnippet)
  :general
  (kdz/leader-insert-def "s" '("Snippet" . consult-yasnippet)))

(use-package embark
  :general
  (general-def
    :keymaps 'vertico-map
    "C-."        '("Act on Candidatae"        . embark-act)      ;; pick some comfortable binding
    "C-;"        '("Act on Candidate (DWIM)"  . embark-dwim)     ;; good alternative: M-.
    "s-<return>" '("Collect Results"          . embark-collect)) ;; 90% of what I want to do is persist results

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

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
  :general
  (kdz/leader-buffer-def
    "s" '("Search Results" . kdz/consult-embark-ripgrep-results))

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

(use-package marginalia
  :init (marginalia-mode)
  :config
  (setq marginalia-align 'right))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :general
  (general-def
    :keymaps 'vertico-map
    "C-j" 'next-line-or-history-element
    "C-h" 'which-key-show-major-mode
    "C-k" 'previous-line-or-history-element)

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
  :ensure nil
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy)))

(use-package consult-todo)

(use-package consult-gh
  :after consult
  :if (executable-find "gh")
  :config
  (setq consult-gh-default-clone-directory "~/dev"))

(use-package consult-gh-embark
  :after consult-gh
  :if (executable-find "gh")
  :config
  (consult-gh-embark-mode +1))

(use-package nerd-icons-completion
  :after (marginalia nerd-icons)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(provide 'packages.d/tools/completion)
