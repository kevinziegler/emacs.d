(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(straight-use-package 'evil)
(straight-use-package 'evil-args)
(straight-use-package 'evil-cleverparens)
(straight-use-package 'evil-collection)
(straight-use-package 'evil-escape)
(straight-use-package 'evil-embrace)
(straight-use-package 'evil-goggles)
(straight-use-package 'evil-lion)
(straight-use-package 'evil-matchit)
(straight-use-package 'evil-nerd-commenter)
(straight-use-package 'evil-numbers)
(straight-use-package 'evil-surround)

;; (straight-use-package 'evil-easymotion)
;; (straight-use-package 'evil-exchange)
;; (straight-use-package 'evil-indent-plus)
;; (straight-use-package 'evil-snipe)
;; (straight-use-package '(evil-textobj-anyblock
;;                         :type git
;;                         :host github
;;                         :repo "willghatch/evil-textobj-anyblock"
;;                         :branch "fix-inner-block"))
;; (straight-use-package 'evil-traces)
;; (straight-use-package 'exato)
;; (straight-use-package '(evil-quick-diff
;;                         :type git
;;                         :host github
;;                         :repo "rgrinberg/evil-quick-diff"))

(straight-use-package 'tabspaces)
(straight-use-package 'which-key)
(straight-use-package 'general)
(straight-use-package 'helpful)
(straight-use-package 'consult)
(straight-use-package 'vertico)
(straight-use-package 'orderless)

(straight-use-package 'smartparens)

(straight-use-package 'treemacs)
(straight-use-package 'treemacs-evil)
(straight-use-package 'treemacs-nerd-icons)

(straight-use-package 'magit)
(straight-use-package 'git-timemachine)
(straight-use-package 'stimmung-themes)
(straight-use-package 'dashboard)
(straight-use-package 'org-modern)
(straight-use-package 'klere-theme)

(straight-use-package 'corfu)
(straight-use-package 'nerd-icons-corfu)
(straight-use-package 'cape)

(straight-use-package 'vertico-posframe)
(straight-use-package 'marginalia)
(straight-use-package 'svg-tag-mode)
(straight-use-package 'spacious-padding)
(straight-use-package 'catppuccin-theme)
(straight-use-package 'solaire-mode)

(straight-use-package 'doom-modeline)

(straight-use-package 'embark)
(straight-use-package 'embark-consult)
(setq straight-use-package-by-default t)
