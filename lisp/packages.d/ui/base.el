(use-package ace-window
  :general
  (kdz/leader-window-def "w" '("Select Window" . ace-window))
  :config
  (ace-window-posframe-mode)
  (set-face-attribute 'aw-leading-char-face nil :height 3.0))

(use-package avy)
(use-package hydra
  :config
  (setq hydra-hint-display-type 'posframe)
  (setq hydra-posframe-show-params
        `(:poshandler posframe-poshandler-frame-center
                      :internal-border-width 2
                      :internal-border-color "#61AFEF"
                      :left-fringe 16
                      :right-fringe 16)))

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

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :config
  (setq nerd-icons-ibuffer-icon t))

(use-package transient
  :config
  (defun kdz/transient-path (file)
    (kdz/user-directory ".local" "transient" file))

  (setq transient-history-file (kdz/transient-path "history.el")
        transient-levels-file  (kdz/transient-path "levels.el")
        transient-values-file  (kdz/transient-path "values.el")))

(use-package ultra-scroll
  :ensure (ultra-scroll :host github :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

;; (use-package casual)

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
    (round
     (* 0.5 (- (plist-get info :parent-frame-width)
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
  (setq treemacs-collapse-dirs 7
        treemacs-width 45
        treemacs-recenter-after-file-follow 'on-distance
        treemacs-project-follow-cleanup t)
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
  :config
  (vertico-posframe-mode 1)
  (setq vertico-posframe-poshandler #'kdz/posframe-offset-top)
  (setq vertico-posframe-parameters '((left-fringe . 8)
                                      (right-fringe . 8))))
(use-package catppuccin-theme
  :after custom
  :hook (elpaca-after-init . (lambda () (catppuccin-load-flavor 'mocha)))
  :config
  (advice-add 'catppuccin-reload
              :after
              (lambda (&rest _) (run-hooks 'kdz-load-theme-hook))))

(use-package creamsody-theme)
(use-package tao-theme)
(use-package stimmung-themes
  :config
  ;; (stimmung-themes-load-light)
  (advice-add 'stimmung-themes-load-light
              :after
              (lambda (&rest _) (run-hooks 'kdz-load-theme-hook)))

  (advice-add 'stimmung-themes-load-dark
              :after
              (lambda (&rest _) (run-hooks 'kdz-load-theme-hook))))

(use-package kaolin-themes
  :config
  (setq kaolin-themes-bold t
        kaolin-themes-distinct-company-scrollbar t
        kaolin-themes-italic t
        kaolin-themes-italic-comments t
        kaolin-themes-underline-wave nil))

(use-package spacious-padding
  :config
  (setq spacious-padding-widths (list :internal-border-width 1
                                      :header-line-width 4
                                      :mode-line-width 6
                                      :tab-width 4
                                      :right-divider-width 7
                                      :scroll-bar-width 0))
  (spacious-padding-mode))

(use-package solaire-mode
  :after catppuccin-theme
  :config
  (solaire-global-mode +1))

(use-package svg-tag-mode)

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :config
  (setq nerd-icons-ibuffer-icon t))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-time nil
	doom-modeline-persp-icon nil
	doom-modeline-persp-name nil
	doom-modeline-buffer-encoding nil))
(provide 'packages.d/ui/base)
