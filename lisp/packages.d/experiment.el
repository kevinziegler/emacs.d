(use-package ace-popup-menu :straight t
  :config (ace-popup-menu-mode 1))

;; TODO Figure out how to enable this conditionally to defer to editorconfig
;;      when available (or, if this is already supported by some built-in
;;      functionality)
(use-package dtrt-indent :straight t)

;; TODO This needs tree-sitter to work.  Somehow this is different than using
;;      (for example) python-ts-mode, which still doesn't seem to mesh with
;;      turbo-log
;; (use-package turbo-log
;;   :straight '(:type git :host github :repo "Artawower/turbo-log")
;;   :config
;;   (setq turbo-log-msg-format-template "\"KDZ-LOG: %s\""))

;; (use-package string-inflection :straight t)
;; (use-package gnuplot :straight t)
;; (use-package wordel :straight t)

;; (use-package which-key-posframe
;;   :straight t
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
  :straight t
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
  :straight t
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;; TODO Set up keybindings
(use-package docsim :straight t
  :config
  (setq docsim-search-paths (list org-directory))
  (defun kdz/docsim-search-current-project ()
    (interactive)
    (if (project-current)
        (let ((docsim-search-paths (project-root (project-current))))
          (call-interactively #'docsim-search)))))

(use-package ialign :straight t)
(use-package wgrep :straight t)
(use-package mistty :straight t)
(use-package grip-mode :straight t)
(use-package auto-virtualenv :straight t)
(use-package projection :straight t)
(use-package json-navigator :straight t)
(use-package focus :straight t)
(use-package better-defaults :straight t)
(use-package duplexer :straight t)
(use-package jwt :straight t)

(use-package scopeline
  :config (add-hook 'prog-mode-hook #'scopeline-mode))

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

(use-package copyit :straight t)
(use-package popwin :straight t)
(use-package git-grep-transient :straight t)
(use-package grip-mode :straight t)
(use-package shx :straight t)
(use-package org-timeblock :straight t)
(use-package color-identifiers-mode :straight t)
(use-package deadgrep :straight t)
(use-package recursion-indicator
  :demand t
  :config
  (recursion-indicator-mode))
(straight-use-package '(ct :host github :repo "neeasade/ct.el" :branch "master"))

(use-package hass :straight t)

(use-package edit-list :straight t)
(use-package test-case-mode :straight t)
(use-package org-linenote :straight t)
(use-package shell-pop :straight t)
(use-package writefreely :straight t)
(use-package buffer-terminator :straight t)
(use-package paredit-mode :straight t)
(use-package enhanced-evil-paredit
  :ensure t
  :config
  (add-hook 'paredit-mode-hook #'enhanced-evil-paredit-mode))

(use-package js-comint :straight t)
(use-package ts-comint :straight t)
(use-package bifocal
  :straight t
  :hook (js-comint-mode . bifocal-mode))

(use-package ztree :straight t)
