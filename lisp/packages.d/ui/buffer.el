;;;; In-Buffer UI Enhancments - Editing behaviors, formating, etc
(use-package browse-at-remote) ;; TODO Set up keybindings
(use-package yasnippet :config (yas-global-mode))
(use-package yasnippet-snippets :after yasnippet)
(use-package apheleia :config (apheleia-global-mode +1))
(use-package anzu :config (global-anzu-mode +1))

(use-package vundo
  :general
  (kdz/leader-buffer-def "h" '("Undo History" . vundo)))

(use-package eval-sexp-fu
  :general
  (kdz/leader-code-eval-def
    "s" '("Evaluate s-exp" . eval-sexp-fu-eval-sexp-inner-list)))

(use-package expand-region
  :ensure (:wait t)
  :general
  (kdz/leader-edit-def "e" '("Expand Region" . er/expand-region)))

(use-package separedit
  :general
  (kdz/leader-edit-def "b" '("Edit block in separate buffer" . separedit)))

(use-package copy-as-format
  :general
  (kdz/leader-edit-def
    "y"   (cons "Copy as <format>" (make-sparse-keymap))
    "yj" '("Copy as JIRA" . copy-as-format-jira)
    "yh" '("Copy as HTML" . copy-as-format-html)
    "ys" '("Copy as Slack" . copy-as-format-slack)
    "ym" '("Copy as Markdown (Plain)" . copy-as-format-markdown)
    "yg" '("Copy as Markdown (Github)" . copy-as-format-github)
    "yG" '("Copy as Markdown (Gitlab)" . copy-as-format-gitlab)))

(use-package jinx
  :general
  (general-def
    :states '(normal)
    :keymaps 'override
    :prefix "z"
    "=" '(jinx-correct :which-key "Correct Spelling"))
  :config
  (global-jinx-mode))

(use-package elec-pair
  :ensure nil
  :hook ((prog-mode . electric-pair-mode)
	 (inferior-emacs-lisp-mode . electric-pair-mode)))

(use-package origami
  :config
  (setq origami-fold-replacement " ... ")
  (global-origami-mode))

(use-package repl-toggle
  :general
  (kdz/leader-code-def "r" '("Toggle REPL" . rtog/toggle-repl))
  :config
  (setq rtog/fullscreen nil
        rtog/fallback-repl-fun #'ielm
	rtog/mode-repl-alist '((emacs-lisp-mode . ielm)
			       (python-mode . run-python)
			       (python-ts-mode . run-python))))

(use-package ws-butler :hook ((prog-mode . ws-butler-mode)))

(use-package editorconfig
  :config
  (setopt editorconfig-trim-whitespaces-mode 'ws-butler-mode)
  (editorconfig-mode 1))

(use-package undo-fu :config (setopt evil-undo-system 'undo-fu))

(use-package markdown-xwidget
  :after markdown-mode
  :ensure (markdown-xwidget
           :host github
           :repo "cfclrk/markdown-xwidget"
           :files (:defaults "resources"))
  :config
  (setq markdown-xwidget-command "pandoc"
        markdown-xwidget-github-theme "light"
        markdown-xwidget-mermaid-theme "default"
        markdown-xwidget-code-block-theme "default") )

;; TODO This needs tree-sitter to work
;; TODO This pulls from quelpa; how do I set that up with straight?
;; (use-package turbo-log
;;   :config
;;   (setq turbo-log-msg-format-template "\"KDZ-LOG: %s\""))

(use-package lorem-ipsum)
(use-package uuidgen)
;; (use-package fancy-urls-menu)

(use-package nocomments-mode
  :general
  (kdz/leader-toggle-def "C" '("Show/Hide comments" . nocomments-mode)))

(use-package sticky-scroll-mode)

(use-package flyover
  :ensure (flyover :host github :repo "konrad1977/flyover" :branch "main"))

(provide 'packages.d/ui/buffer)
