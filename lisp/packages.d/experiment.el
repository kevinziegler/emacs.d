(use-package ace-popup-menu :config (ace-popup-menu-mode 1))

;; TODO Figure out how to enable this conditionally to defer to editorconfig
;;      when available (or, if this is already supported by some built-in
;;      functionality)
(use-package dtrt-indent)

;; TODO This needs tree-sitter to work.  Somehow this is different than using
;;      (for example) python-ts-mode, which still doesn't seem to mesh with
;;      turbo-log
;; (use-package turbo-log
;;   :straight '(:type git :host github :repo "Artawower/turbo-log")
;;   :config
;;   (setq turbo-log-msg-format-template "\"KDZ-LOG: %s\""))

;; (use-package string-inflection)
;; (use-package gnuplot)
;; (use-package wordel)

;; (use-package which-key-posframe
;;  
;;   :config
;;   ;; Stand-in until the following issue is merged:
;;   ;; https://github.com/yanghaoxie/which-key-posframe/pull/21
;;   ;;
;;   ;; Note that my version *also* tweaks the width parameter
;;   (defun kdz/fixup--which-key-posframe--show-buffer (act-popup-dim)
;;     "Override which-key-posframe parameters to ensure content is visible"
;;     (when (posframe-workable-p)
;;       (save-window-excursion
;;         (posframe-show
;;          which-key--buffer
;;          :font which-key-posframe-font
;;          :position (point)
;;          :poshandler which-key-posframe-poshandler
;;          :background-color (face-attribute 'which-key-posframe :background nil t)
;;          :foreground-color (face-attribute 'which-key-posframe :foreground nil t)
;;          :height (ceiling (* 1.25 (car act-popup-dim)))
;;          :width (ceiling (* 1.1 (cdr act-popup-dim)))
;;          :internal-border-width which-key-posframe-border-width
;;          :internal-border-color (face-attribute 'which-key-posframe-border
;;                                                 :background nil
;;                                                 t)
;;          :override-parameters which-key-posframe-parameters))))

;;   (advice-add #'which-key-posframe--show-buffer
;;               :override
;;               #'kdz/fixup--which-key-posframe--show-buffer)

;;   (setq which-key-posframe-poshandler 'kdz/posframe-offset-bottom)
;;   (which-key-posframe-mode 1))

;; TODO I like this package, but I need to figure out how to better integrate it
;;      into my workflow.
(use-package mindstream
  :config
  (setq mindstream-path (kdz/user-directory ".local/mindstream"))
  (mindstream-mode))

(defun kdz/transient-context-menu-entry-to-transient (menu-entry)
  (if (eq (car menu-entry) 'keymap)
      (message "FIXME")
    (let ((entry-type (nth 2 menu-entry))))))

(defun kdz/transient-context-menu-at-point ()
  "Generate a transient menu for the current context menu options"
  (let ((context (context-menu-map)))
    ;; TODO Make this work:
    ;;      - Need to parse `menu-item' items from the context menu into actions
    ;;        that can be executed here
    ;;      - Need to generate context sub-menu structures into transient's
    ;;        data structure
    (transient-define-prefix context-menu-transient ()
      "Context Menu"
      [[(car context)
        :pad-keys t
        ]])))


(use-package yasnippet-capf
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;; TODO Set up keybindings
(use-package docsim
  :config
  (setq docsim-search-paths (list org-directory))
  (defun kdz/docsim-search-current-project ()
    (interactive)
    (if (project-current)
        (let ((docsim-search-paths (project-root (project-current))))
          (call-interactively #'docsim-search)))))

(use-package ialign)
(use-package wgrep)
(use-package mistty)
(use-package grip-mode)
(use-package auto-virtualenv)
(use-package projection)
(use-package json-navigator)
(use-package focus)
(use-package better-defaults)
(use-package duplexer)
(use-package jwt)

(use-package scopeline :hook (prog-mode . scopeline-mode))

(defun kdz/get-posframe-for-buffer (buffer-or-name)
  "Get the posframe for BUFFER-OR-STRING, if it exists",
  (when-let ((buffer (get-buffer buffer-or-name)))
    (seq-find (lambda (frame)
                (eq buffer (cdr (frame-parameter frame 'posframe-buffer))))
              (frame-list))))

(defvar kdz-show-posframe-below-space-between 10
  "Space between two posframes when stacking one below another")

(defun kdz/show-posframe-below (buffer-or-string
                                other-posframe
                                &optional space-between)
  "Show a posframe for BUFFER-OR-STRING below OTHER-POSFRAME"
  ;; TODO Make this posframe (optionally?) inherit properties from OTHER-POSFRAME
  (if other-posframe
      (let* ((other-position (frame-position other-posframe))
             (other-width (frame-pixel-width other-posframe))
             (other-height (frame-pixel-height other-posframe))
             (spacing (or space-between kdz-show-posframe-below-space-between))
             (new-position (cons (car other-position)
                                 (+ (cdr other-position) other-height spacing))))
        (posframe-show buffer-or-string
                       :position new-position
                       :posfame-width other-width))
    (message "No OTHER-POSFRAME supplied!")))

(use-package evil-expat
  :ensure t
  ;; optional, defer loading until 1 second of inactivity,
  ;; hence not affecting emacs startup time
  :defer 1)

(use-package copyit)
(use-package popwin)
(use-package git-grep-transient)
(use-package grip-mode)
(use-package shx)
(use-package org-timeblock)
(use-package color-identifiers-mode)
(use-package deadgrep)
(use-package recursion-indicator :demand t :config (recursion-indicator-mode))
(straight-use-package '(ct :host github :repo "neeasade/ct.el" :branch "master"))

(use-package hass)

(use-package edit-list)
(use-package test-case-mode)
(use-package org-linenote)
(use-package shell-pop)
(use-package writefreely)
(use-package buffer-terminator)
(use-package paredit-mode)
(use-package enhanced-evil-paredit
  :hook (paredit-mode . enhanced-evil-paredit-mode))

(use-package js-comint)
(use-package ts-comint)
(use-package bifocal
  :hook (js-comint-mode . bifocal-mode))

(use-package ztree)

(use-package nerd-icons-multimodal
  :straight (:host github :repo "abougouffa/nerd-icons-multimodal")
  :hook ((archive-mode tar-mode dired-mode) . nerd-icons-multimodal-mode))

(use-package mood-line
  
  ;; Enable mood-line
  :config
  (setq mood-line-format mood-line-format-default)
  (mood-line-mode))

(use-package consult-omni
  :ensure (consult-omni :host github
                        :repo "armindarvish/consult-omni"
                        :branch "main"
                        :files (:defaults "sources/*.el"))
  :after consult
  :config
  ;; Load Sources Core code
  (require 'consult-omni-sources)
  ;; Load Embark Actions
  (require 'consult-omni-embark)
  (consult-omni-sources-load-modules)
  ;;; Set your shorthand favorite interactive command
  (setq consult-omni-default-interactive-command #'consult-omni-brave-autosuggest))

;; TODO Why does this give an error with 'firefox?
(use-package browser-hist)
(use-package link-hint)

;; TODO Remember which buffers I've dealt with in the past that use ansi color sequences and see if this can't help display them properly?
(use-package xterm-color)
(use-package shackle)
(use-package org-special-block-extras)
(use-package no-littering)
(use-package tab-bar-notch)
(use-package tab-line-nerd-icons)
(use-package smart-delete)
(use-package on-parens)
(use-package better-scroll)
(use-package centered-window)
(use-package buffer-terminator)
(use-package org-linenote)
(use-package lispy)
(use-package test-case-mode)

;; TODO Figure out what modes hook up
(use-package auto-rename-tag)

(use-package tabgo)
(use-package nocomments-mode)

(use-package scopeline
  :config (add-hook 'prog-mode-hook #'scopeline-mode))

(use-package org-timeblock)
(use-package org-table-color)

(use-package consult-gh)
(use-package beacon
  :delight beacon-mode
  :custom
  (beacon-color "#33DB12")
  (beacon-blink-duration 0.5)
  (beacon-size 60)
  (beacon-blink-when-point-moves-vertically t)
  (beacon-blink-when-window-changes t)
  (beacon-blink-when-focused t)
  :config
  (beacon-mode)
  ;; blink after switching windows.
  (if window-selection-change-functions
      (push (lambda (_) (beacon-blink-automated)) 'window-selection-change-functions)
    (setq window-selection-change-functions '((lambda (_) (beacon-blink-automated))))))

(use-package citre)
(use-package enlight)
