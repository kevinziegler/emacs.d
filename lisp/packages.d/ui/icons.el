(use-package nerd-icons
  :after emacs
  :config
  (defvar kdz-nerd-icons-mapped-prefixes
    '(("iec"     . nerd-icons-ipsicon)
      ("seti"    . nerd-icons-sucicon)
      ("custom"  . nerd-icons-sucicon)
      ("weather" . nerd-icons-wicon))
    "Mapping of nf-<iconset> names to their nerd-icons-* symbol prefixes.

This is to handle cases where the symbol name cannot be constructed from the
string name programmatically")

  (defun kdz/nerd-icons-prefix-for-name (icon-name &optional as-symbol)
    "Find the correct symbol prefix for ICON-NAME."
    (let* ((family-str (nth 1 (s-split "-" icon-name)))
           (mapped-prefix (kdz/alist-str-get family-str
                                             kdz-nerd-icons-mapped-prefixes)))
      (if mapped-prefix
          (if as-symbol mapped-prefix (symbol-name mapped-prefix))
        (let ((prefix (concat "nerd-icons-" family-str "icon")))
          (if as-symbol (intern prefix) prefix)))))

  (defun kdz/nerd-icons-function-for-name (icon-name)
    "Return the function for rendering ICON-NAME"
    (kdz/nerd-icons-prefix-for-name icon-name t))

  (defun kdz/nerd-icons-family-for-name (icon-name)
    "Return the font family for ICON-NAME"
    (funcall (intern (concat (kdz/nerd-icons-prefix-for-name icon-name)
                             "-family"))))

  (defun kdz/nerd-icons-properties-for (icon-name)
    "Make a list of default text properties for ICON-NAME.

This list is meant to be used as a base when calling #'propertize for the icon
referenced by ICON-NAME."
    (let* ((face-family-props
            `(:family ,(kdz/nerd-icons-family-for-name icon-name) :height 1.2)))
      (list 'face face-family-props
            'font-lock-face face-family-props
            'display '(raise 0))))

  (defun kdz/nerd-icons-dwim (name)
    "Get a nerd-icons icon with name, regardless of icon set"
    (funcall (kdz/nerd-icons-function-for-name name) name))

  (defun kdz/propertize-nerd-icon (name &optional properties)
    (apply #'propertize
           `(,(kdz/nerd-icons-dwim name)
             ,@(kdz/plist-merge (kdz/nerd-icons-properties-for name) properties)))))

(use-package nerd-icons-completion
  :after (marginalia nerd-icons)
  :hook ((elpaca-after-init . nerd-icons-completion-mode)
         (marginalia-mode . nerd-icons-completion-marginalia-setup)))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :after (dired nerd-icons)
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-grep
  :init
  (nerd-icons-grep-mode)
  :custom
  ;; This setting is a pre-requirement, so an icon can be displayed near each
  ;; heading
  (grep-use-headings t))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :custom (nerd-icons-ibuffer-icon t))

(use-package nerd-icons-xref
  :hook (elpaca-after-init . nerd-icons-xref-mode))

(provide 'packages.d/ui/icons)
