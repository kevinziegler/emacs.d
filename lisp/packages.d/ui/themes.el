;;; -*- lexical-binding: t -*-
(use-package kaolin-themes
  :custom
  (kaolin-themes-bold t)
  (kaolin-themes-distinct-company-scrollbar t)
  (kaolin-themes-italic t)
  (kaolin-themes-italic-comments t)
  (kaolin-themes-underline-wave nil)
  :config
  (defun kdz/kaolin-dark-theme-custom-faces (theme)
    (when (eq theme 'kaolin-dark)
      (message "In guard handler")
      (kdz/customize-with-palette
       'kaolin-dark
       'kaolin-palette
       `(tab-bar                   ,(kdz/tab-bar-face-spec-base (color 'black0)
                                                                (color 'auquamarine0)))
       `(tab-bar-tab                ((t :foreground ,(color 'aquamarine0) :weight bold)))
       `(tab-bar-tab-inactive       ((t :inherit tab-bar
                                        :background ,(color 'black0))))
       '(tab-line                   ((t :inherit tab-bar)))
       '(tab-line-tab               ((t :inherit tab-bar-tab)))
       `(tab-line-tab-inactive      ((t :inherit tab-line
                                        :foreground ,(color 'white0)
                                        :background ,(color 'black0))))
       `(tab-line-tab-special       ((t :inherit tab-line
                                        :foreground ,(color 'white1)
                                        :background ,(color 'black0)))))))
  (add-hook 'enable-theme-functions 'kdz/kaolin-dark-theme-custom-faces))

(use-package modus-themes
  :hook ((elpaca-after-init . (lambda () (load-theme 'modus-operandi :no-confirm))))
  :init
  (setopt modus-themes-bold-constructs t
          modus-themes-prompts '(bold)
          modus-themes-italic-constructs t)
  :config
  (defun kdz/modus-customizations (theme)
    (when (or (string-prefix-p "modus-" (symbol-name theme))
              (string-prefix-p "doric-" (symbol-name theme))
              (string-prefix-p "ef-" (symbol-name theme)))
      (kdz/customize-with-palette
       theme
       (intern (concat (symbol-name theme) "-palette"))
       `(aw-leading-char-face       ((t :foreground ,(color 'cyan-intense)
                                        :background ,(color 'bg-active)
                                        :height 2.5)))
       `(child-frame-border         ((t :background ,(color 'border)
                                        :foreground ,(color 'border))))
       `(eldoc-box-border           ((t :background ,(color 'fg-main))))
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
       `(header-line                ((t :inherit modus-themes-ui-variable-pitch
                                        :box (:line-width 3 :color ,(color 'bg-dim))
                                        :background ,(color 'bg-dim))))
       `(minibuffer-prompt          ((t :box (:line-width 10 :color ,(color 'bg-main)))))
       `(mode-line                  ((t :background ,(color 'bg-ochre)
                                        :box (:line-width 6 :color ,(color 'bg-ochre)))))
       `(mode-line-active           ((t :background ,(color 'bg-ochre)
                                        :box (:line-width 6 :color ,(color 'bg-ochre)))))
       `(window-divider             ((t :foreground ,(color 'fg-main))))
       `(window-divider-first-pixel ((t :foreground ,(color 'fg-main))))
       `(window-divider-last-pixel  ((t :foreground ,(color 'fg-main))))
       `(tab-bar                    ((t :box (:line-width 10 :color ,(color 'bg-main))
                                        :underline (:color ,(color 'fg-main) :position -10))))
       `(tab-bar-tab                ((t :foreground ,(color 'fg-main) :weight bold)))
       `(tab-bar-tab-inactive       ((t :inherit tab-bar
                                        :background ,(color 'bg-main))))
       '(tab-line                   ((t :inherit tab-bar)))
       '(tab-line-active            ((t :inherit tab-bar)))
       '(tab-line-inactive          ((t :inherit tab-bar)))
       '(tab-line-tab               ((t :inherit tab-bar-tab)))
       `(tab-line-tab-inactive      ((t :inherit tab-line
                                        :foreground ,(color 'fg-dim)
                                        :background ,(color 'bg-main))))
       `(tab-line-tab-special       ((t :inherit tab-line
                                        :foreground ,(color 'cyan-intense)
                                        :background ,(color 'bg-main))))
       `(tab-line-tab-current       ((t :inherit tab-line-tab
                                        :weight bold
                                        :foreground ,(color 'fg-main)))))))
  (add-hook 'enable-theme-functions 'kdz/modus-customizations))

(use-package modus-flexoki
  :ensure (:host github :repo "dpassen/modus-flexoki" :branch "main")
  :hook (elpaca-after-init . (lambda () (load-theme 'modus-flexoki-light :no-confirm))))

;; (use-package modus-catppuccin
;;   :ensure (:host gitlab :repo "magus/modus-catppuccin" :branch "main")
;;   :hook (elpaca-after-init . (lambda () (load-theme 'catpuccin-mocha :no-confirm))))


(use-package doric-themes)
(use-package ef-themes)

(provide 'packages.d/ui/themes)
