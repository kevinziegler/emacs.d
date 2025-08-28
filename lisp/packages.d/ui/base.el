(use-package ace-window
  :general
  (kdz/leader-window-def "w" '("Select Window" . ace-window))
  :config
  (ace-window-posframe-mode)
  (set-face-attribute 'aw-leading-char-face nil :height 3.0))

(use-package avy)

(use-package minimal-dashboard
  :ensure (minimal-dashboard :host github
                             :repo "dheerajshenoy/minimal-dashboard.el")
  :after project
  :preface
  (setq initial-buffer-choice #'minimal-dashboard)
  :config
  (setopt minimal-dashboard-buffer-name "*dashboard*"
          minimal-dashboard-enable-resize-handling t
          minimal-dashboard-text "Welcome."
          minimal-dashboard-image-path (expand-file-name "logo.svg"
                                                         user-emacs-directory))

  ;; TODO Figure out why the dashboard buffer gets killed (or buried)
  ;;      immediately after launching `project-find-file' (instead of running
  ;;      `kdz/kill-dashboard' after finishing the selection process)
  (defun kdz/kill-dashboard ()
    (with-current-buffer minimal-dashboard-buffer-name
      (kill-buffer-and-window))))

(use-package display-fill-column-indicator
  :ensure nil
  :init
  (global-display-fill-column-indicator-mode)
  :general
  (kdz/leader-toggle-def "c"
    '("Show/hide fill column" . display-fill-column-indicator-mode))

  (kdz/leader-toggle-global-def "c"
    '("Show/hide fill column" . global-display-fill-column-indicator-mode))

  :config
  (setopt display-fill-column-indicator-character ?\u2502)
  (dolist (mode '(dired-mode
                  dirvish-directory-view-mode
                  helpful-mode
                  markdown-mode
                  org-mode
                  special-mode
                  xref--xref-buffer-mode))
    (add-to-list 'global-display-fill-column-indicator-modes `(not ,mode))))

(use-package display-line-numbers
  :ensure nil
  :init
  (global-display-line-numbers-mode)
  :general
  (kdz/leader-toggle-def
    "l" '("Show/hide line numbers"         . display-line-numbers-mode)
    "r" '("Relative/absolute line numbers" . kdz/toggle-line-numbers))
  (kdz/leader-toggle-global-def
    "l" '("Show/hide line numbers" . global-display-line-numbers-mode))

  :config
  (defun kdz/toggle-line-numbers ()
    "Cycle between relative/absolute line numbers"
    (interactive)
    (if display-line-numbers
        (setq display-line-numbers
	      (if (eq display-line-numbers 'relative) t 'relative))
      (message "Line numbers are currently disabled!")))

  (dolist (mode '(dashboard-mode-hook
                  dired-mode
                  dirvish-directory-view-mode
                  org-mode-hook
                  term-mode-hook
                  treemacs-mode-hook
                  eshell-mode-hook
                  xref--xref-buffer-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode -1)))))

(use-package elfeed
  :config
  (setq elfeed-search-feed-face ":foreground #ffffff :weight bold"
        elfeed-feeds '(("https://planet.emacslife.com/atom.xml" devtools emacs)
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
  :general
  (kdz/leader-file-def "i" '("Show Info" . file-info-show))
  :config
  (setq file-info-headline-underline-symbol ?━))

(use-package flycheck
  :config
  (setopt flycheck-checker-error-threshold 10000))

(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode 1))

(use-package hide-mode-line
  :hook ((reb-mode . hide-mode-line-mode)))

;; TODO Need to adjust face colors to contrast properly
(use-package highlight-indent-guides
  :general
  (kdz/leader-toggle-def
    "i" '("Show/hide indent guides" . highlight-indent-guides-mode)))

(use-package hl-todo
  :after custom
  :hook (kdz-load-theme . kdz/set-hl-todo-faces)
  :config
  (defun kdz/set-hl-todo-faces (&rest _)
    "Set face colors for hl-todo keywords

This is performed via a function so it can be used as a hook on
actions that would update colors in emacs (such as changing themes)"
    (setq hl-todo-keyword-faces
          `(("TODO"   . ,(face-foreground 'hl-todo))
            ("FIXME"  . ,(face-foreground 'ansi-color-red))
            ("DEBUG"  . ,(face-foreground 'ansi-color-cyan))
            ("NOTE"   . ,(face-foreground 'ansi-color-blue))
            ("STUB"   . ,(face-foreground 'ansi-color-green)))))
  (global-hl-todo-mode))

(use-package hydra
  :config
  (setq hydra-hint-display-type 'posframe)
  (setq hydra-posframe-show-params
        `(:poshandler posframe-poshandler-frame-center
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
  :config
  (fringe-mode)
  (modern-fringes-mode)
  (modern-fringes-invert-arrows))

(use-package nerd-icons
  :config
  (defvar kdz-nerd-icons-function-map
    '(("nf-cod"     . nerd-icons-codicon)
      ("nf-dev"     . nerd-icons-devicon)
      ("nf-fa"      . nerd-icons-faicon)
      ("nf-iec"     . nerd-icons-ipsicon)
      ("nf-md"      . nerd-icons-mdicon)
      ("nf-oct"     . nerd-icons-octicon)
      ("nf-pom"     . nerd-icons-pomicon)
      ("nf-seti"    . nerd-icons-sucicon)
      ("nf-custom"  . nerd-icons-sucicon)
      ("nf-weather" . nerd-icons-wicon))
    "Mapping of nf-<iconset> names to their nerd-icons-* function ")

  (defvar kdz-nerd-icons-family-function-map
    '(("nf-cod"     . nerd-icons-codicon-family)
      ("nf-dev"     . nerd-icons-devicon-family)
      ("nf-fa"      . nerd-icons-faicon-family)
      ("nf-iec"     . nerd-icons-ipsicon-family)
      ("nf-md"      . nerd-icons-mdicon-family)
      ("nf-oct"     . nerd-icons-octicon-family)
      ("nf-pom"     . nerd-icons-pomicon-family)
      ("nf-seti"    . nerd-icons-sucicon-family)
      ("nf-custom"  . nerd-icons-sucicon-family)
      ("nf-weather" . nerd-icons-wicon-family))
    "Mapping of nf-<iconset> names to their nerd-icons-* function ")

  (defun kdz/find-by-prefix-key (prefix-alist name)
    (cdr (seq-find (lambda (mapping) (s-starts-with? (car mapping) name))
                   prefix-alist)))

  (defun kdz/nerd-icons-dwim (name)
    "Get a nerd-icons icon with name, regardless of icon set"
    (funcall (kdz/find-by-prefix-key kdz-nerd-icons-function-map name) name))

  (defun kdz/propertize-nerd-icon (name &optional properties)
    (let* ((face-family-fn
            (kdz/find-by-prefix-key kdz-nerd-icons-family-function-map name))
           (face-family (funcall face-family-fn))
           (face-family-props (list :family face-family :height 1.2))
           (base-properties (list 'face face-family-props
                                  'font-lock-face face-family-props
                                  'display '(raise 0)))
           (evaluated-properties
            (map-merge-with 'plist
                            (lambda (base extra)
                              (if (and (listp base) (listp extra))
                                  (map-merge 'plist base extra)
                                extra))
                            base-properties
                            properties)))
      (apply #'propertize
             `(,(kdz/nerd-icons-dwim name) ,@evaluated-properties)))))

(use-package nerd-icons-dired
  :after (dired nerd-icons)
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :config
  (setq nerd-icons-ibuffer-icon t))

(use-package perfect-margin)

(use-package scroll-on-jump
  :after evil
  :config
  (setq scroll-on-jump-curve 'smooth-in)
  (setq scroll-on-jump-duration 0.6)
  (setq scroll-on-jump-curve-power 3.5)

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
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(use-package solaire-mode
  :after catppuccin-theme
  :config
  (solaire-global-mode +1))

(use-package svg-tag-mode)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-time nil
        doom-modeline-persp-icon nil
        doom-modeline-persp-name nil
        doom-modeline-buffer-encoding nil))

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
                      (* offset-percent parent-frame-height)))))))

(use-package treemacs
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
  (setq treemacs-collapse-dirs 0
        treemacs-width 45
        treemacs-recenter-after-file-follow 'on-distance
        treemacs-project-follow-cleanup t)
  (treemacs-git-mode 0)
  (treemacs-hide-gitignored-files-mode 1))

(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-tab-bar
  :after treemacs
  :config (treemacs-set-scope-type 'Tabs))

(use-package treemacs-evil :after (treemacs evil))

(use-package vertico-posframe
  :after (posframe vertico)
  :hook (elpaca-after-init . vertico-posframe-mode)
  :custom
  (vertico-posframe-poshandler #'kdz/posframe-offset-top)
  (vertico-posframe-parameters '((left-fringe . 8) (right-fringe . 8))))

(use-package spacious-padding
  :config
  (setq spacious-padding-widths (list :internal-border-width 2
                                      :header-line-width     4
                                      :mode-line-width       6
                                      :tab-width             0
                                      :right-divider-width   2
                                      :scroll-bar-width      0))
  (spacious-padding-mode))

(use-package sideline
  :general
  (kdz/leader-toggle-def
    "s" '("Show/hide Sideline" . sideline-mode))
  (kdz/leader-toggle-global-def
    "s" '("Show/hide Sideline" . global-sideline-mode)))

(use-package sideline-blame
  :after 'sideline
  :config (add-to-list 'sideline-backends-right sideline-blame))

(use-package sideline-flycheck
  :after '(sideline flycheck)
  :config (add-to-list 'sideline-backends-right sideline-flycheck))

(use-package colorful-mode)

(use-package project-rootfile
  :config
  (add-to-list 'project-rootfile-list ".project"))

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
  :general
  (kdz/leader-toggle-def "k" '("Keycast Display" . keycast-tab-bar-mode)))

(use-package eldoc-box
  :general
  (kdz/leader-toggle-def "d" '("Documentation Popups" . eldoc-box-hover-mode))
  :custom
  (eldoc-box-only-multi-line t)
  (eldoc-box-position-function #'eldoc-box--default-at-point-position-function))

(use-package helpful
  :general
  (kdz/leader-help-def
    "c" '("Describe Command"        . helpful-command)
    "f" '("Describe Callable"       . helpful-callable)
    "h" '("Describe Thing-at-point" . helpful-at-point)
    "v" '("Describe Variable"       . helpful-variable)
    "k" '("Describe Key"            . helpful-key)
    "s" '("Describe Symbol"         . helpful-symbol)))

(provide 'packages.d/ui/base)
