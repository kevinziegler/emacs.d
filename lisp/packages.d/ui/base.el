(use-package avy)
(use-package colorful-mode)
(use-package flycheck :custom (flycheck-checker-error-threshold 10000))
(use-package hide-mode-line :hook ((reb-mode . hide-mode-line-mode)))
(use-package perfect-margin)
(use-package project-rootfile :config (add-to-list 'project-rootfile-list ".project"))
(use-package solaire-mode :hook (elpaca-after-init . solaire-global-mode))
(use-package treemacs-nerd-icons :after treemacs :config (treemacs-load-theme "nerd-icons"))
(use-package treemacs-tab-bar :after treemacs :config (treemacs-set-scope-type 'Tabs))
(use-package treemacs-evil :after (treemacs evil))

(use-package ace-window
  :general (kdz/leader-window-def "w" '("Select Window" . ace-window))
  :hook (elpaca-after-init . ace-window-posframe-mode))

(use-package breadcrumb
  :custom
  (breadcrumb-imenu-crumb-separator " ❱ ")
  :hook (elpaca-after-init . breadcrumb-mode))

(use-package minimal-dashboard
  :ensure
  (minimal-dashboard :host github :repo "dheerajshenoy/minimal-dashboard.el")
  :preface
  (setq minimal-dashboard-image-path nil
        initial-buffer-choice #'minimal-dashboard)
  :custom
  (minimal-dashboard-buffer-name "*dashboard*")
  (minimal-dashboard-enable-resize-handling t)
  (minimal-dashboard-text "Welcome.")
  :config
  ;; TODO Figure out why the dashboard buffer gets killed (or buried)
  ;;      immediately after launching `project-find-file' (instead of running
  ;;      `kdz/kill-dashboard' after finishing the selection process)
  (defun kdz/kill-dashboard ()
    (with-current-buffer minimal-dashboard-buffer-name (kill-buffer-and-window))))

(use-package elfeed
  :custom
  (elfeed-search-feed-face ":foreground #ffffff :weight bold")
  (elfeed-feeds '(("https://planet.emacslife.com/atom.xml" devtools emacs)
                  ("https://www.reddit.com/r/austin.rss" reddit austin)
                  ("https://www.reddit.com/r/commandline.rss" reddit devtools)
                  ("https://www.reddit.com/r/emacs.rss" reddit devtools emacs)
                  ("https://www.reddit.com/r/orgmode.rss" reddit devtools emacs)
                  ("https://www.reddit.com/r/zsh.rss" reddit devtools)
                  ("https://hackaday.com/blog/feed/" tech news)
                  ("https://news.ycombinator.com/rss" tech news)
                  ("https://www.commitstrip.com/en/feed/" webcomic)
                  ("https://xkcd.com/rss.xml" webcomic))))

(use-package file-info
  :custom (file-info-headline-underline-symbol ?━)
  :general (kdz/leader-file-def "i" '("Show Info" . file-info-show)))

(use-package git-gutter-fringe
  :custom
  (git-gutter:added-sign    "┃")
  (git-gutter:modified-sign "┃")
  (git-gutter:deleted-sign  "⎽")
  :hook
  (elpaca-after-init . global-git-gutter-mode))

(use-package highlight-indent-guides
  :general (kdz/leader-toggle-def "i" '("Indent guides" . highlight-indent-guides-mode)))

(use-package hl-todo
  :hook ((elpaca-after-init . global-hl-todo-mode))
  :config
  (add-to-list 'hl-todo-keyword-faces '("DEBUG" .  "#d0bf8f"))
  (add-to-list 'hl-todo-keyword-faces '("STUB"  .  "#7cb8bb")))

(use-package hydra
  :custom
  (hydra-hint-display-type 'posframe)
  (hydra-posframe-show-params (list :poshandler posframe-poshandler-frame-center
                                    :internal-border-width 2
                                    :internal-border-color "#61AFEF"
                                    :left-fringe 16
                                    :right-fringe 16)))

(use-package imenu-list
  :general
  (general-def
    :keymaps 'imenu-list-major-mode-map
    "s-<return>" 'imenu-list-goto-entry)
  (kdz/leader-code-lookup-def "l" '("Symbols List" . imenu-list)))

(use-package pretty-hydra
  :after hydra
  :config
  (pretty-hydra-define
    kdz-pretty-window-resize
    (:foreign-keys warn
                   :title (format "Resize window for: %s (Step size: %d)"
                                  (or (buffer-name) "N/A")
                                  kdz-window-resize-step--current)
                   :quit-key "q"
                   :color pink)
    ("Resize Window"
     (("j" kdz/window-dec-height              "Decrease Height")
      ("k" kdz/window-inc-height              "Increase Height")
      ("h" kdz/window-dec-width               "Decrease Width")
      ("l" kdz/window-inc-width               "Increase Width"))
     "Fit Window"
     (("w" kdz/window-fit-to-buffer-width     "Fit to buffer width")
      ("f" kdz/window-fit-to-buffer-fill      "Fit to buffer fill-column")
      ("R" kdz/window-restore-original-width  "Restore to original width"))
     "Select Window"
     (("J" evil-window-down                   "Select window down")
      ("K" evil-window-up                     "Select window down")
      ("H" evil-window-left                   "Select window down")
      ("L" evil-window-right                  "Select window down"))
     "Resize Step"
     (("r" kdz/window-step-size-set-or-reset  "(Re)set Step")
      ("=" kdz/window-step-size-inc           "Increase Step Size")
      ("+" kdz/window-step-size-inc           "Increase Step Size")
      ("_" kdz/window-step-size-dec           "Decrease Step Size")
      ("-" kdz/window-step-size-dec           "Decrease Step Size")))))

(use-package modern-fringes
  :hook
  (elpaca-after-init . fringe-mode)
  (elpaca-after-init . modern-fringes-mode)
  (elpaca-after-init . modern-fringes-invert-arrows))

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

(use-package nerd-icons-dired
  :after (dired nerd-icons)
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :custom (nerd-icons-ibuffer-icon t))

(use-package scroll-on-jump
  :after evil
  :custom
  (scroll-on-jump-curve 'smooth-in)
  (scroll-on-jump-duration 0.6)
  (scroll-on-jump-curve-power 3.5)
  :config
  (scroll-on-jump-advice-add evil-undo)
  (scroll-on-jump-advice-add evil-redo)
  (scroll-on-jump-advice-add evil-jump-item)
  (scroll-on-jump-advice-add evil-jump-forward)
  (scroll-on-jump-advice-add evil-jump-backward)
  (scroll-on-jump-advice-add evil-ex-search-next)
  (scroll-on-jump-advice-add evil-ex-search-previous)
  (scroll-on-jump-advice-add evil-forward-paragraph)
  (scroll-on-jump-advice-add evil-backward-paragraph)
  (scroll-on-jump-advice-add evil-goto-mark)

  ;; Actions that themselves scroll.
  (scroll-on-jump-with-scroll-advice-add evil-goto-line)
  (scroll-on-jump-with-scroll-advice-add evil-goto-first-line)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-down)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-up)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-center)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-top)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-bottom))

(use-package ultra-scroll
  :ensure (ultra-scroll :host github :repo "jdtsmith/ultra-scroll")
  :custom
  (scroll-conservatively 101); important!
  (scroll-margin 0)
  :hook (elpaca-after-init . ultra-scroll-mode))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-time nil)
  (doom-modeline-persp-icon nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-vcs-icon t)
  (doom-modeline-vcs-display-function nil)
  :config
  (defvar kdz-modeline-buffer-name-maps '(("*IList*" . "Symbols")))

  (defun kdz/imenu-list-buffer-display-name (buffer-or-name)
    (with-current-buffer buffer-or-name
      (format "Symbols (%s)" (buffer-file-name imenu-list--displayed-buffer))))

  (defun kdz/modeline-buffer-name (buffer)
    (let ((override (cdr (assoc buffer kdz-modeline-buffer-name-maps))))
      (cond ((stringp override) override)
            ((functionp overide) (funcall override buffer))
            ((t buffer))))))

(use-package posframe
  :config
  (defvar kdz--posframe-offset-top-percent 10)
  (defvar kdz--posframe-offset-bottom-percent 10)

  (defun kdz/posframe-center-width (info)
    (round (* 0.5 (- (plist-get info :parent-frame-width)
                     (plist-get info :posframe-width)))))

  (defun kdz/posframe-offset-top (info)
    (let ((offset-percent (/ kdz--posframe-offset-top-percent 100.0))
          (frame-height (plist-get info :parent-frame-height)))
      (cons (kdz/posframe-center-width info)
            (round (* offset-percent frame-height)))))

  (defun kdz/posframe-offset-bottom (info)
    (let* ((parent-frame-height (plist-get info :parent-frame-height))
           (posframe-height (plist-get info :posframe-height))
           (offset-percent (/ kdz--posframe-offset-bottom-percent 100.0)))
      (cons (kdz/posframe-center-width info)
            (round (- parent-frame-height
                      posframe-height
                      (* offset-percent parent-frame-height))))))

  (defun kdz/display-in-posframe-bottom (buffer alist)
    "Show BUFFER in a posframe at the bottom of the frame."
    (posframe-show buffer
                   :poshandler #'kdz/posframe-offset-bottom
                   :border-color (face-foreground 'child-frame-border))))

(use-package transient-posframe
  :after (transient posframe)
  :custom (transient-posframe-poshandler #'posframe-poshandler-window-bottom-right-corner)
  :hook (elpaca-after-init . transient-posframe-mode))

(use-package treemacs
  :custom
  (treemacs-collapse-dirs 0)
  (treemacs-width 45)
  (treemacs-recenter-after-file-follow 'on-distance)
  (treemacs-project-follow-cleanup t)
  :general
  (general-def
    :keymaps 'treemacs-mode-map
    :prefix "o"
    "v" 'treemacs-visit-node-horizontal-split
    "h" 'treemacs-visit-node-vertical-split
    "s" 'treemacs-visit-node-vertical-split)

  (general-def
    :keymaps 'treemacs-mode-map
    :prefix "o a"
    "v" 'treemacs-visit-node-ace-horizontal-split
    "h" 'treemacs-visit-node-ace-vertical-split
    "s" 'treemacs-visit-node-ace-vertical-split)

  (kdz/leader-open-def "t" '("Project File Tree" . treemacs))
  :config
  (treemacs-git-mode 0)
  (treemacs-hide-gitignored-files-mode 1))

(use-package vertico-posframe
  :after (posframe vertico)
  :hook (elpaca-after-init . vertico-posframe-mode)
  :custom
  (vertico-posframe-poshandler #'kdz/posframe-offset-top)
  (vertico-posframe-parameters '((left-fringe . 8) (right-fringe . 8))))

(use-package spacious-padding
  :custom
  (spacious-padding-widths (list :internal-border-width 2
                                 :header-line-width     4
                                 :mode-line-width       6
                                 :tab-width             0
                                 :right-divider-width   2
                                 :scroll-bar-width      0))
  :hook (elpaca-after-init . spacious-padding-mode))

(use-package sideline
  :general
  (kdz/leader-toggle-def "s" '("Show/hide Sideline" . sideline-mode))
  (kdz/leader-toggle-global-def "s" '("Show/hide Sideline" . global-sideline-mode)))

(use-package sideline-blame
  :after sideline
  :config (add-to-list 'sideline-backends-right sideline-blame))

(use-package sideline-flycheck
  :after (sideline flycheck)
  :config (add-to-list 'sideline-backends-right sideline-flycheck))

(use-package ibuffer-project
  :hook (ibuffer . kdz/ibuffer-tune-sort-and-filter)
  :config
  (defun kdz/ibuffer-tune-sort-and-filter ()
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative))))

(use-package otpp
  :after project
  :hook ((elpaca-after-init . otpp-mode)
         (elpaca-after-init . otpp-override-mode)))

(use-package keycast
  :general (kdz/leader-toggle-def "k" '("Keycast Display" . keycast-tab-bar-mode)))

(use-package eldoc-box
  :general
  (kdz/leader-toggle-def "d" '("Documentation Popups" . eldoc-box-hover-mode))
  :hook (eglot-managed-mode . eldoc-box-hover-mode)
  :custom
  (eldoc-box-only-multi-line t)
  (eldoc-box-position-function eldoc-box--default-at-point-position-function))

(use-package helpful
  :hook (helpful-mode . kdz/helpful-header)
  :general
  (kdz/leader-help-def
    "c" '("Describe Command"        . helpful-command)
    "f" '("Describe Callable"       . helpful-callable)
    "h" '("Describe Thing-at-point" . helpful-at-point)
    "v" '("Describe Variable"       . helpful-variable)
    "k" '("Describe Key"            . helpful-key)
    "s" '("Describe Symbol"         . helpful-symbol))

  :config
  (defun kdz/helpful-header ()
    (setq-local header-line-format
                '(:eval (concat  (propertize "Help: " 'face `(:weight bold))
                                 (symbol-name helpful--sym))))))

(use-package casual
  :after (calc calendar)
  :defer t
  :general
  (kdz/mode-leader-def :keymaps 'calc-mode-map "m" '("Menu" . casual-calc-tmenu))
  (kdz/mode-leader-def :keymaps 'calc-alg-map "m" '("Menu" . casual-calc-tmenu))
  (kdz/mode-leader-def :keymaps 'calendar-mode-map "m" '("Menu" . casual-calendar))
  (kdz/mode-leader-def :keymaps 'ediff-mode-map "m" '("Menu" . casual-ediff-tmenu))

  (Man-mode-map "n" #'casual-lib-browse-forward-paragraph
                "p" #'casual-lib-browse-backward-paragraph
                "[" #'Man-previous-section
                "]" #'Man-next-section
                "j" #'next-line
                "k" #'previous-line
                "K" #'Man-kill
                "o" #'casual-man-occur-options)
  (reb-mode-map "C-o" #'casual-re-builder-tmenu)
  (reb-lisp-mode-map "C-o" #'casual-re-builder-tmenu)
  :config
  ;; TODO These are overidden by evil-mode keybindings.  Move to general ]
  ;;      configurations?
  ;; (keymap-set calc-mode-map "C-o" #'casual-calc-tmenu)
  ;; (keymap-set calc-alg-map "C-o" #'casual-calc-tmenu)
  ;; (keymap-set calendar-mode-map "C-o" #'casual-calendar)

  ;; (add-hook 'ediff-keymap-setup-hook
  ;;           (lambda ()
  ;;             (keymap-set ediff-mode-map "C-o" #'casual-ediff-tmenu)))
  ;; run this to enable Casual Ediff
  (casual-ediff-install))

(provide 'packages.d/ui/base)
