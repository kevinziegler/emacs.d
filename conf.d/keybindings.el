(require 'general)
 
(general-def
  :states 'normal
  :keymaps 'override
  :prefix "SPC b"
  :non-normal-prefix "M-SPC b"
  ""  '(:ignore t                  :which-key "Buffer")
  "b" '(tabspaces-switch-to-buffer :which-key "Switch to Buffer (Workspace)")
  "B" '(switch-to-buffer           :which-key "Switch to Buffer (Global)")
  "d" '(kill-current-buffer        :which-key "Kill Buffer")
  "h" '(vundo                      :which-key "Undo History")
  "n" '(next-buffer                :which-key "Next Buffer")
  "p" '(previous-buffer            :which-key "Previous Buffer")
  "N" '(evil-buffer-new            :which-key "New Buffer"))

(general-def
  :states 'normal 
  :prefix "SPC c"
  :non-normal-prefix "M-SPC c"
  ""  '(:ignore t        :which-key "Code")
  ;; TODO Gate this behind a predicate
  "r" '(rtog/toggle-repl :which-key "Toggle REPL"))

(general-def
  :states 'normal 
  :keymaps 'override
  :prefix "SPC f"
  :non-normal-prefix "M-SPC f"
  ""  '(:ignore t      :which-key "File")
  "l" '(org-store-link :which-key "Store Link")
  "f" '(find-file      :which-key "Find File"))

(general-def
  :states '(normal treemacs)
  :keymaps 'override
  :prefix "SPC h"
  :non-normal-prefix "M-SPC h"
  ""  '(:ignore t        :which-key "Help")
  "e" '(list-environment :which-key "List Environment Variables")
  "m" '(describe-mode    :which-key "Describe Mode")
  "f" '(helpful-callable :which-key "Describe Callable")
  "v" '(helpful-variable :which-key "Describe Variable")
  "k" '(helpful-key      :which-key "Describe Key"))

(general-def
  :states '(normal treemacs)
  :prefix "SPC o"
  :non-normal-prefix "M-SPC o"
  ""  '(:ignore t  :which-key "Open")
  "f" '(make-frame :which-key "New Frame")
  "t" '(treemacs   :which-key "Project File Tree"))

(general-def
  :states '(normal treemacs)
  :prefix "SPC g"
  :non-normal-prefix "M-SPC g"
  ""  '(:ignore t             :which-key "Git")
  "g" '(magit-status          :which-key "Git Status")
  "t" '(git-timemachine       :which-key "Time Machine")
  "b" '(magit-blame           :which-key "Blame File")
  "l" '(magit-log-buffer-file :which-key "Log for File")
  "m" '(nil                   :which-key "Merge Conflicts"))

(general-def
  :states '(normal treemacs)
  :keymaps 'override
  :prefix "SPC w"
  :non-normal-prefix "M-SPC w"
  ""  '(:ignore t            :which-key "Window")
  "h" '(evil-window-left     :which-key "Left")
  "j" '(evil-window-down     :which-key "Down")
  "k" '(evil-window-up       :which-key "Up")
  "l" '(evil-window-right    :which-key "Right")
  "d" '(evil-window-delete   :which-key "Delete Window")
  "T" '(tear-off-window      :which-key "Tear off Window")
  "n" '(evil-window-new      :which-key "New Window")
  "x" '(evil-window-exchange :which-key "Exchange Window"))

(general-def
  :states '(normal treemacs)
  :keymaps 'override
  :prefix "SPC p"
  :non-normal-prefix "M-SPC p"
  ""  '(:ignore t                                      :which-key "Project")
  "a" '(project-remember-projects-under                :which-key "Add Projects")
  "D" '(project-forget-project                         :which-key "Remove Project")
  "f" '(project-find-file                              :which-key "Open Project File")
  "p" '(tabspaces-open-or-create-project-and-workspace :which-key "Switch To Project"))

(general-def
  :states '(normal treemacs)
  :keymaps 'override
  :prefix "SPC TAB"
  :non-normal-prefix "M-SPC TAB"
  ""    '(:ingore t                               :which-key "Workspace")
  "TAB" '(kdz/tab-switch-index-or-select          :which-key "Select Workspace")
  "h"   '(tab-previous                            :which-key "Previous Workspace")
  "l"   '(tab-next                                :which-key "Next Workspace")
  "H"   '((lambda () (interactive) (tab-move -1)) :which-key "Move Tab Left")
  "L"   '(tab-move                                :which-key "Move Tab Right")
  "d"   '(tabspaces-close-workspace               :which-key "Close Workspace")
  "n"   '(kdz/create-named-tab                    :which-key "Create Named Workspace"))

(general-def
  :states '(normal treemacs)
  :keymaps 'override
  :prefix "SPC t"
  :non-normal-prefix "M-SPC t"
  ""  '(:ignore t                                 :which-key "Toggle")
  "i" '(highlight-indent-guides-mode              :which-key "Show/Hide Indent Guides")
  "l" '(display-line-numbers-mode                 :which-key "Show/Hide Line Numbers")
  "r" '(kdz/toggle-line-numbers                   :which-key "Relative/Absolute Line Numbers")
  "c" '(global-display-fill-column-indicator-mode :which-key "Show/Hide Fill Column"))

(general-def
  :states 'normal
  :keymaps 'override
  :prefix "SPC s"
  :non-normal-prefix "M-SPC s"
  ""  '(:ignore t                :which-key "Search")
  "p" '(consult-ripgrep          :which-key "Project")
  "t" '(tap/consult-ripgrep-dwim :which-key "Thing-at-point (DWIM)")
  "s" '(tap/consult-ripgrep      :which-key "Thing-at-point (Select)"))

(general-def
  :states 'normal
  :keymaps 'org-mode-map
  :prefix "SPC i"
  :non-normal-prefix "M-SPC i"
  ""  '(:ignore t                          :which-key "Insert")
  "h" '(org-insert-heading-respect-content :which-key "Insert Heading")
  "s" '(org-insert-subheading              :which-key "Insert Sub-heading")
  "l" '(org-mac-link-get-link              :which-key "Mac Link"))

(general-def
  :states '(normal visual treemacs)
  :keymaps 'override
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  "u"   '(universal-argument             :which-key "Apply Universal Argument")
  ";"   '(pp-eval-expression             :which-key "Evaluate Expression")
  "SPC" '(execute-extended-command       :which-key "Execute Command"))


(provide 'keybindings)
;;; keybindings.el ends here
