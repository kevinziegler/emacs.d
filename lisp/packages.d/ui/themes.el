(use-package catppuccin-theme
  :after custom
  :hook (
         ;;(elpaca-after-init . (lambda () (load-theme 'catppuccin :no-confirm)))
         (kdz-load-theme . kdz/catppuccin-theme-custom-faces))
  :custom (catppuccin-flavor 'mocha)
  :config
  (defun kdz/catppuccin-theme-custom-faces ()
    (kdz/customize-with-palette
     'catppuccin
     'catppuccin-color
     `(highlight             ((t :foreground ,(color 'base)
                                 :background ,(color 'flamingo))))
     `(lazy-highlight        ((t :foreground ,(color 'base)
                                 :background ,(color 'flamingo))))
     `(completions-highlight ((t :background ,(color 'surface1))))
     `(vertico-current       ((t :background ,(color 'surface1))))
     `(minibuffer-prompt     ((t :weight bold :foreground ,(color 'sapphire))))
     `(tab-bar              ,(kdz/tab-bar-face-spec-base
                              (face-background 'tab-bar-tab nil t)
                              (face-foreground 'tab-bar-tab nil t)))
     `(tab-bar-tab           ((t :weight bold)))
     '(tab-bar-tab-inactive  ((t :inherit tab-bar)))
     '(tab-line              ((t :inherit tab-bar)))
     '(tab-line-tab          ((t :inherit tab-bar-tab)))
     '(tab-line-tab-current  ((t :inherit tab-bar-tab))))))

(use-package kaolin-themes
  :custom
  (kaolin-themes-bold t)
  (kaolin-themes-distinct-company-scrollbar t)
  (kaolin-themes-italic t)
  (kaolin-themes-italic-comments t)
  (kaolin-themes-underline-wave nil))

(use-package stimmung-themes
  :config
  (defun kdz/stimmung-theme-custom-faces ()
    (kdz/customize-with-palette
     'stimmung-themes-light
     nil
     '(git-gutter:added      ((t :foreground "systemGreenColor")))
     '(git-gutter:deleted    ((t :foreground "systemRedColor")))
     '(git-gutter:added      ((t :foreground "systemYellowColor")))
     '(git-gutter-fr:added   ((t :foreground "systemGreenColor")))
     '(git-gutter-fr:deleted ((t :foreground "systemRedColor")))
     '(git-gutter-fr:added   ((t :foreground "systemYellowColor")))
     `(tab-bar              ,(kdz/tab-bar-face-spec-base "White" "Black"))
     '(tab-bar-tab           ((t :foreground "Black" :weight bold)))
     '(tab-bar-tab-inactive  ((t :inherit tab-bar)))
     '(tab-line              ((t :inherit tab-bar)))
     '(tab-line-tab          ((t :inherit tab-bar-tab)))
     '(tab-line-tab-current  ((t :inherit tab-bar-tab))))
    (kdz/customize-with-palette
     'stimmung-themes-dark
     nil
     '(git-gutter:added      ((t :foreground "systemGreenColor")))
     '(git-gutter:deleted    ((t :foreground "systemRedColor")))
     '(git-gutter:added      ((t :foreground "systemYellowColor")))
     '(git-gutter-fr:added   ((t :foreground "systemGreenColor")))
     '(git-gutter-fr:deleted ((t :foreground "systemRedColor")))
     '(git-gutter-fr:added   ((t :foreground "systemYellowColor")))
     `(tab-bar              ,(kdz/tab-bar-face-spec-base "Black" "White"))
     '(tab-bar-tab           ((t :foreground "White" :weight bold)))
     '(tab-bar-tab-inactive  ((t :inherit tab-bar)))
     '(tab-line              ((t :inherit tab-bar)))
     '(tab-line-tab          ((t :inherit tab-bar-tab)))
     '(tab-line-tab-current  ((t :inherit tab-bar-tab))))))

(use-package modus-themes
  :hook ((elpaca-after-init . (lambda () (load-theme 'modus-operandi-tritanopia :no-confirm)))
         (kdz-load-theme    . kdz/modus-operandi-tritanopia-customizations))
  :init
  (setopt modus-themes-bold-constructs t
          modus-themes-italic-constructs t)
  :config
  (defun kdz/modus-operandi-tritanopia-customizations ()
    (kdz/customize-with-palette
     'modus-operandi-tritanopia
     'modus-operandi-tritanopia-palette
     `(aw-leading-char-face       ((t :foreground ,(color 'red) :height 3.0)))
     `(child-frame-border         ((t :background ,(color 'border)
                                      :foreground ,(color 'border))))
     `(flyover-marker             ((t :foreground ,(color 'bg-cyan-intense))))
     `(fill-column-indicator      ((t :background ,(color 'bg-main)
                                      :foreground ,(color 'border))))
     `(git-gutter:added           ((t :foreground ,(color 'bg-added-fringe)
                                      :background ,(color 'bg-main))))
     `(git-gutter:deleted         ((t :foreground ,(color 'bg-removed-fringe)
                                      :background ,(color 'bg-main))))
     `(git-gutter:modified        ((t :foreground ,(color 'bg-changed-fringe)
                                      :background ,(color 'bg-main))))
     `(git-gutter-fr:added        ((t :foreground ,(color 'bg-added-fringe)
                                      :background ,(color 'bg-main))))
     `(git-gutter-fr:deleted      ((t :foreground ,(color 'bg-removed-fringe)
                                      :background ,(color 'bg-main))))
     `(git-gutter-fr:modified     ((t :foreground ,(color 'bg-changed-fringe)
                                      :background ,(color 'bg-main))))
     `(window-divider             ((t :foreground ,(color 'border))))
     `(window-divider-first-pixel ((t :foreground ,(color 'border))))
     `(tab-bar                   ,(kdz/tab-bar-face-spec-base (color 'bg-main)
                                                              (color 'fg-main)))
     `(tab-bar-tab                ((t :foreground ,(color 'fg-main) :weight bold)))
     `(tab-bar-tab-inactive       ((t :inherit tab-bar
                                      :background ,(color 'bg-main))))
     '(tab-line                   ((t :inherit tab-bar)))
     '(tab-line-tab               ((t :inherit tab-bar-tab)))
     `(tab-line-tab-inactive      ((t :inherit tab-bar-tab
                                      :foreground ,(color 'fg-dim)
                                      :background ,(color 'bg-main))))
     `(tab-line-tab-current       ((t :inherit tab-bar-tab
                                      :weight bold
                                      :foreground ,(color 'fg-main)))))))

(provide 'packages.d/ui/themes)
