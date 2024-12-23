(general-def
  :states '(normal insert emacs)
  :keymaps 'override
  :prefix "SPC b"
  :non-normal-prefix "M-SPC b"
  ""  '(:ignore t                           :which-key "Buffer")
  "b" '(project-switch-to-buffer            :which-key "Switch to Buffer (Workspace)")
  "B" '(switch-to-buffer                    :which-key "Switch to Buffer (Global)")
  "c" '(kdz/switch-to-buffer-current-window :which-key "Switch to Buffer (Force current window)")
  "d" '(kill-current-buffer                 :which-key "Kill Buffer")
  "h" '(vundo                               :which-key "Undo History")
  "n" '(next-buffer                         :which-key "Next Buffer")
  "p" '(previous-buffer                     :which-key "Previous Buffer")
  "N" '(evil-buffer-new                     :which-key "New Buffer")
  "s" '(kdz/consult-embark-ripgrep-results  :which-key "Search Results"))

(general-def
  :states '(normal insert emacs)
  :prefix "SPC c"
  :non-normal-prefix "M-SPC c"
  ""  '(:ignore t        :which-key "Code")
  ;; TODO Gate this behind a predicate
  "r" '(rtog/toggle-repl :which-key "Toggle REPL"))

(general-def
  :states '(normal insert emacs)
  :prefix "SPC c l"
  :non-normal-prefix "M-SPC c l"
  ""  '(:ignore t             :which-key "Lookup Symbols")
  "l" '(lsp-treemacs-symbols  :which-key "Symbols List")
  "d" '(xref-find-definitions :which-key "Lookup Definition")
  "r" '(xref-find-references  :which-key "Lookup References"))

(general-def
  :states '(normal visual insert emacs)
  :keymaps 'emacs-lisp-mode-map
  :prefix "SPC c e"
  :non-normal-prefix "M-SPC c e"
  ;; TODO Gate this behind a predicate
  ""  '(:ignore t                         :which-key "Code Evaluation")
  "b" '(eval-buffer                       :which-key "Evaluate Buffer")
  "d" '(eval-defun                        :which-key "Evaluate Function")
  "r" '(eval-region                       :which-key "Evaluate Region")
  "s" '(eval-sexp-fu-eval-sexp-inner-list :which-key "Evaluate S-Exp"))

(general-def
  :states '(normal visual insert emacs)
  :prefix "SPC e"
  :non-normal-prefix "M-SPC e"
  ""  '(:ignore t        :which-key "Edit")
  "a" '(align-regexp     :which-key "Align by Regexp")
  "b" '(separedit        :which-key "Edit block in separate buffer")
  "e" '(er/expand-region :which-key "Expand Region"))

(general-def
  :states '(normal visual insert emacs)
  :prefix "SPC e y"
  :non-normal-prefix "M-SPC e y"
  ""  '(:ignore t            :which-key "Copy as <format>")
  "j" '(copy-as-format-jira  :which-key "Copy as JIRA")
  "h" '(copy-as-format-html  :which-key "Copy as HTML")
  "s" '(copy-as-format-slack :which-key "Copy as Slack")
  "m" '(copy-as-format-markdown :which-key "Copy as Markdown (Plain)")
  "g" '(copy-as-format-github   :which-key "Copy as Markdown (Github)")
  "G" '(copy-as-format-gitlab   :which-key "Copy as Markdown (Gitlab)"))

(general-def
  :states '(normal visual insert emacs)
  :keymaps 'org-mode-map
  :prefix "SPC e"
  :non-normal-prefix "M-SPC e"
  "b" '(org-edit-special :which-key "Edit Org Block"))

(general-def
  :states '(normal insert emacs)
  :keymaps 'override
  :prefix "SPC f"
  :non-normal-prefix "M-SPC f"
  ""  '(:ignore t                          :which-key "File")
  "i" '(file-info-show                     :which-key "Show Info")
  "l" '(org-store-link                     :which-key "Store Link")
  "f" '(find-file                          :which-key "Find File")
  "r" '(crux-rename-file-and-buffer        :which-key "Rename File")
  "D" '(crux-delete-file-and-buffer        :which-key "Delete File")
  "c" '(crux-copy-file-preserve-attributes :which-key "Copy File"))

(general-def
  :states '(normal insert emacs treemacs)
  :keymaps 'override
  :prefix "SPC h"
  :non-normal-prefix "M-SPC h"
  ""  '(:ignore t        :which-key "Help")
  "e" '(list-environment :which-key "List Environment Variables")
  "m" '(man              :which-key "Lookup Manpage")
  "M" '(describe-mode    :which-key "Describe Mode")
  "f" '(helpful-callable :which-key "Describe Callable")
  "v" '(helpful-variable :which-key "Describe Variable")
  "k" '(helpful-key      :which-key "Describe Key"))

(general-def
  :states '(normal insert emacs treemacs)
  :keymaps 'override
  :prefix "SPC h d"
  :non-normal-prefix "M-SPC h d"
  ""  '(:ignore t       :which-key "Dev Docs")
  "d" '(devdocs-search  :which-key "Search in Documentation")
  "p" '(devdocs-peruse  :which-key "Peruse Documentation")
  "i" '(devdocs-install :which-key "Download Documentation Set"))

(general-def
  :states '(normal insert emacs)
  :keymaps 'override
  :prefix "SPC i"
  :non-normal-prefix "M-SPC i"
  ""  '(:ignore t         :which-key "Insert")
  "u" '(insert-char       :which-key "Unicode Character")
  "s" '(consult-yasnippet :which-key "Snippet"))

(general-def
  :states '(normal insert emacs treemacs)
  :prefix "SPC o"
  :non-normal-prefix "M-SPC o"
  ""  '(:ignore t            :which-key "Open")
  "f" '(make-frame           :which-key "New Frame")
  "F" '(reveal-in-osx-finder :which-key "Finder Window (Current Directory)")
  "s" '(terminal-here        :which-key "Terminal (Current Directory)")
  "t" '(treemacs             :which-key "Project File Tree"))

(general-def
  :states '(normal insert visual emacs treemacs)
  :keymaps 'override
  :prefix "SPC g"
  :non-normal-prefix "M-SPC g"
  ""  '(:ignore t             :which-key "Git")
  "g" '(magit-status          :which-key "Git Status")
  "t" '(git-timemachine       :which-key "Time Machine")
  "b" '(magit-blame           :which-key "Blame File")
  "l" '(magit-log-buffer-file :which-key "Log for File")
  "m" '(nil                   :which-key "Merge Conflicts"))

(general-def
  :states '(normal insert visual emacs treemacs)
  :keymaps 'override
  :prefix "SPC g y"
  :non-normal-prefix "M-SPC g y"
  ""  '(:ignore t                :which-key "Copy Remote Link")
  "h" '(git-link-homepage        :which-key "Repository Homepage")
  "y" '(git-link                 :which-key "File + Line Number")
  "Y" '(kdz/git-link-with-commit :which-key "File + Line Number (@ Commit)"))

(general-def
  :states '(normal insert emacs treemacs)
  :keymaps 'override
  :prefix "SPC w"
  :non-normal-prefix "M-SPC w"
  ""  '(:ignore t                         :which-key "Window")
  "=" '(balance-windows                   :which-key "Balance Windows")
  "h" '(kdz/window-left-dwim              :which-key "Left")
  "j" '(evil-window-down                  :which-key "Down")
  "k" '(evil-window-up                    :which-key "Up")
  "l" '(kdz/window-right-dwim             :which-key "Right")
  "d" '(evil-window-delete                :which-key "Delete Window")
  "f" '(kdz/window-fit-to-buffer-width    :which-key "Fit to buffer width")
  "F" '(kdz/window-fit-to-buffer-fill     :which-key "Fit to buffer fill")
  "r" '(kdz/window-restore-original-width :which-key "Restore window width")
  "s" '(kdz-pretty-window-resize/body     :which-key "Resize Window")
  "T" '(tear-off-window                   :which-key "Tear off Window")
  "n" '(evil-window-new                   :which-key "New Window")
  "x" '(evil-window-exchange              :which-key "Exchange Window")
  "w" '(ace-window                        :which-key "Select Window")
  "H" '(evil-window-move-far-left         :which-key "Move window left")
  "L" '(evil-window-move-far-right        :which-key "Move window right")
  "J" '(evil-window-move-far-down         :which-key "Move window down")
  "K" '(evil-window-move-far-up           :which-key "Move window up"))

(general-def
  :states '(normal insert emacs treemacs)
  :keymaps 'override
  :prefix "SPC p"
  :non-normal-prefix "M-SPC p"
  ""  '(:ignore t                       :which-key "Project")
  "a" '(project-remember-projects-under :which-key "Add Projects")
  "D" '(project-forget-project          :which-key "Remove Project")
  "e" '(lsp-treemacs-errors-list        :which-key "Project Errors")
  "f" '(project-find-file               :which-key "Open Project File")
  "p" '(project-switch-project          :which-key "Switch To Project"))

(general-def
  :states '(normal insert emacs treemacs)
  :keymaps 'override
  :prefix "SPC TAB"
  :non-normal-prefix "M-SPC TAB"
  ""    '(:ingore t                               :which-key "Workspace")
  "TAB" '(kdz/tab-switch-index-or-select          :which-key "Select Workspace")
  "h"   '(tab-previous                            :which-key "Previous Workspace")
  "l"   '(tab-next                                :which-key "Next Workspace")
  "H"   '((lambda () (interactive) (tab-move -1)) :which-key "Move Tab Left")
  "L"   '(tab-move                                :which-key "Move Tab Right")
  "d"   '(tab-bar-close-tab  :which-key "Close Workspace")
  "n"   '(kdz/create-named-tab                    :which-key "Create Named Workspace"))

(general-def
  :states '(normal insert emacs treemacs)
  :keymaps 'override
  :prefix "SPC t"
  :non-normal-prefix "M-SPC t"
  ""  '(:ignore t                          :which-key "Toggle")
  "c" '(display-fill-column-indicator-mode :which-key "Show/Hide Fill Column")
  "i" '(highlight-indent-guides-mode       :which-key "Show/Hide Indent Guides")
  "l" '(display-line-numbers-mode          :which-key "Show/Hide Line Numbers")
  "r" '(kdz/toggle-line-numbers            :which-key "Relative/Absolute Line Numbers")
  "s" '(sideline-mode                      :which-key "Show/Hide Sideline"))

(general-def
  :states '(normal insert emacs treemacs)
  :keymaps 'override
  :prefix "SPC t g"
  :non-normal-prefix "M-SPC t g"
  ""  '(:ignore t                                 :which-key "Toggle (Global)")
  "c" '(global-display-fill-column-indicator-mode :which-key "Show/Hide Fill Column")
  "k" '(keycast-tab-bar-mode                      :which-key "Keycast Display")
  "l" '(global-display-line-numbers-mode          :which-key "Show/Hide Line Numbers")
  "s" '(global-sideline-mode                      :which-key "Show/Hide Sideline"))

(general-def
  :states '(normal insert emacs)
  :keymaps 'override
  :prefix "SPC s"
  :non-normal-prefix "M-SPC s"
  ""  '(:ignore t                    :which-key "Search")
  "*" '(kdz/consult-ripgrep-selected :which-key "Search for selection")
  "l" '(consult-line                 :which-key "Search for Line (Current Buffer)")
  "L" '(consult-line-multi           :which-key "Search for Line (Project Buffers)")
  "p" '(consult-ripgrep              :which-key "Project")
  "r" '(re-builder                   :which-key "Regexp Builder")
  "s" '(tap/consult-ripgrep          :which-key "Thing-at-point (Select)")
  "t" '(tap/consult-ripgrep-dwim     :which-key "Thing-at-point (DWIM)"))

(general-def
  :states 'normal
  :keymaps 'org-mode-map
  :prefix "SPC i o"
  :non-normal-prefix "M-SPC i o"
  ""    '(:ignore t                          :which-key "Org-Mode")
  "c"   '(kdz/org-make-toc-dwim              :which-key "Insert Table of Contents")
  "h"   '(org-insert-heading-respect-content :which-key "Insert Heading")
  "s"   '(org-insert-subheading              :which-key "Insert Sub-heading")
  "l"   '(org-mac-link-get-link              :which-key "Insert Link (From Application)"))

(general-def
  :states 'normal
  :prefix "SPC j"
  :non-normal-prefix "M-SPC j"
  "" '(:ignore t             :which-key "Jump to <thing>")
  "j" '(evil-avy-goto-line   :which-key "Jump to line")
  "c" '(evil-avy-goto-char   :which-key "Jump to character")
  "w" '(evil-avy-goto-word-0 :which-key "Jump to word"))

(general-def
  :states '(normal visual treemacs)
  :keymaps 'override
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  "u"   '(universal-argument             :which-key "Apply Universal Argument")
  ";"   '(pp-eval-expression             :which-key "Evaluate Expression")
  "RET" '(context-menu-open              :which-key "Show Context Menu")
  "SPC" '(execute-extended-command       :which-key "Execute Command"))

(general-def
  :keymaps 'vertico-map
  "C-j" 'next-line-or-history-element
  "C-k" 'previous-line-or-history-element)

(general-def
  :states '(normal)
  :keymaps 'override
  :prefix "z"
  "=" '(jinx-correct :which-key "Correct Spelling"))

(general-def
  :states '(normal visual)
  "gc" 'evilnc-comment-operator)

(general-def
  :states 'insert
  :keymaps 'org-mode-map
  "s-<return>" 'kdz/org-return-dwim)

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

(general-def
  :states 'normal
  "+" '(evil-numbers/inc-at-pt :which-key "Increment at point")
  "=" '(evil-numbers/inc-at-pt :which-key "Increment at point")
  "-" '(evil-numbers/dec-at-pt :which-key "Decrement at point")
  "_" '(evil-numbers/dec-at-pt :which-key "Decrement at point"))

(provide 'conf.d/keybindings)
;;; keybindings.el ends here
