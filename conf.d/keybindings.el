;;; keybindings.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Kevin Ziegler
;;
;; Author: Kevin Ziegler <kevinziegler@Balder.local>
;; Maintainer: Kevin Ziegler <kevinziegler@Balder.local>
;; Created: September 23, 2023
;; Modified: September 23, 2023
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'general)
 
(general-def
  :states '(normal visual)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  "a" '(kdz-buffer-actions-map :which-key "Testing Buffer Actions")
  "u" '(universal-argument :which-key "Apply Universal Argument")
  ";" '(pp-eval-expression :which-key "Evaluate Expression")
  "SPC" '(execute-extended-command :which-key "Execute Command"))

(defvar kdz-buffer-actions-map (make-sparse-keymap))
(general-def
  :keymaps 'kdz-buffer-actions-map
  "b" '(tabspaces-switch-to-buffer :which-key "Switch to Buffer")
  "d" '(kill-current-buffer :which-key "Kill Buffer")
  "n" '(next-buffer :which-key "Next Buffer")
  "p" '(previous-buffer :which-key "Previous Buffer")
  "N" '(evil-buffer-new :which-key "New Buffer"))

(general-def
  :states '(normal visual)
  :prefix "SPC b"
  :non-normal-prefix "M-SPC b"
  ""  '(:ingore t :which-key "Buffer")
  "b" '(tabspaces-switch-to-buffer :which-key "Switch to Buffer")
  "d" '(kill-current-buffer :which-key "Kill Buffer")
  "n" '(next-buffer :which-key "Next Buffer")
  "p" '(previous-buffer :which-key "Previous Buffer")
  "N" '(evil-buffer-new :which-key "New Buffer"))

(general-def
  :states '(normal visual)
  :prefix "SPC h"
  :non-normal-prefix "M-SPC h"
  ""  '(:ingore t :which-key "Help")
  "m" '(describe-mode :which-key "Describe Mode")
  "f" '(helpful-callable :which-key "Describe Callable")
  "v" '(helpful-variable :which-key "Describe Variable")
  "k" '(helpful-key :which-key "Describe Key"))

(general-def
  :states 'normal
  :prefix "SPC o"
  :non-normal-prefix "M-SPC o"
  ""  '(:ingore t :which-key "Open")
  "f" '(make-frame :which-key "New Frame")
  "t" '(treemacs :which-key "Project File Tree"))

(general-def
  :states 'normal
  :prefix "SPC f"
  :non-normal-prefix "M-SPC f"
  ""  '(:ingore t :which-key "File")
  "f" '(find-file :which-key "Find File"))

(general-def
  :states 'normal
  :prefix-map 'leader-menu-git
  :prefix "SPC g"
  :non-normal-prefix "M-SPC g"
  ""  '(:ingore t :which-key "Git")
  "g" '(magit-status :which-key "Git Status")
  "t" '(git-timemachine :which-key "Time Machine")
  "b" '(magit-blame :which-key "Blame File")
  "l" '(magit-log-buffer-file :which-key "Log for File")
  "m" '(nil :which-key "Merge Conflicts"))

(general-def
  :states 'normal
  :prefix "SPC w"
  :non-normal-prefix "M-SPC w"
  ""  '(:ingore t :which-key "Window")
  "h" '(evil-window-left :which-key "Left")
  "j" '(evil-window-down :which-key "Down")
  "k" '(evil-window-up :which-key "Up")
  "l" '(evil-window-right :which-key "Right")
  "d" '(evil-window-delete :which-key "Delete Window")
  "T" '(tear-off-window :which-key "Tear off Window")
  "n" '(evil-window-new :which-key "New Window")
  "x" '(evil-window-exchange :which-key "Exchange Window"))

(general-def
  :states 'normal
  :prefix "SPC p"
  :non-normal-prefix "M-SPC p"
  ""  '(:ingore t :which-key "Project")
  "a" '(project-remember-projects-under :which-key "Add Projects")
  "D" '(project-forget-project :which-key "Remove Project")
  "f" '(project-find-file :which-key "Open Project File")
  "p" '(tabspaces-open-or-create-project-and-workspace :which-key "Switch To Project"))

(general-def
  :states 'normal
  :prefix "SPC TAB"
  :non-normal-prefix "M-SPC TAB"
  ""    '(:ingore t :which-key "Workspace")
  "TAB" '(kdz/tab-switch-index-or-select :which-key "Select Workspace")
  "h"   '(tab-previous :which-key "Previous Workspace")
  "l"   '(tab-next :which-key "Next Workspace")
  "H"   '((lambda () (interactive) (tab-move -1)) :which-key "Move Tab Left")
  "L"   '(tab-move :which-key "Move Tab Right")
  "d"   '(tabspaces-close-workspace :which-key "Close Workspace"))

(general-def
  :states 'normal
  :prefix "SPC t"
  :non-normal-prefix "M-SPC t"
  ""  '(:ignore t :which-key "Toggle")
  "i" '(highlight-indent-guides-mode :which-key "Show/Hide Indent Guides")
  "l" '(display-line-numbers-mode :which-key "Show/Hide Line Numbers")
  "r" '(kdz/toggle-line-numbers :which-key "Relative/Absolute Line Numbers")
  "c" '(global-display-fill-column-indicator-mode :which-key "Show/Hide Fill Column"))

(general-def
  :states 'normal
  :prefix "SPC s"
  :non-normal-prefix "M-SPC s"
  ""  '(:ignore t :which-key "Search")
  "p" '(consult-ripgrep :which-key "Search Project"))

(provide 'keybindings)
;;; keybindings.el ends here
