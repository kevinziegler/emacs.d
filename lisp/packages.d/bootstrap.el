;; (defvar bootstrap-version)
;; (setq straight-base-dir (kdz/user-directory ".local"))

;; (let ((bootstrap-file (kdz/user-directory ".local"
;;                                           "straight"
;;                                           "repos"
;;                                           "straight.el"
;;                                           "bootstrap.el"))
;;       (bootstrap-version 6))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; (straight-use-package 'use-package)
;; (setq straight-use-package-by-default t)

(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (kdz/user-directory ".local" "elpaca"))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))

(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

(elpaca-wait)

;; Set up `general' here so the `:general' keyword is available for use in
;; `use-package' declarations later in my config
(use-package general
  :demand t
  :config
  (general-define-key
   :states '(normal insert motion emacs)
   :keymaps 'override
   :prefix-map 'kdz-leader-map
   :prefix "SPC"
   :non-normal-prefix "M-SPC")

  (general-create-definer kdz/leader-def
    :keymaps 'kdz-leader-map)
  (kdz/leader-def "" nil)

  (general-create-definer kdz/mode-leader-def
    :states '(normal insert motion emacs)
    :keymaps 'override
    :major-modes t
    :prefix "SPC m"
    :non-normal-prefix "M-SPC m")
  (kdz/mode-leader-def "" nil)

  (defmacro kdz/leader-nested-map-init (name)
    `(progn
       (defvar-keymap ,(intern (format "kdz-%s-actions-map" name)))
       (general-create-definer ,(intern (format "kdz/leader-%s-def" name))
         :keymaps ',(intern (format "kdz-%s-actions-map" name)))
       (,(intern (format "kdz/leader-%s-def" name)) "" nil)))

  (kdz/leader-nested-map-init "buffer")
  (kdz/leader-nested-map-init "code")
  (kdz/leader-nested-map-init "code-eval")
  (kdz/leader-nested-map-init "code-lookup")
  (kdz/leader-nested-map-init "edit")
  (kdz/leader-nested-map-init "file")
  (kdz/leader-nested-map-init "git")
  (kdz/leader-nested-map-init "help")
  (kdz/leader-nested-map-init "insert")
  (kdz/leader-nested-map-init "open")
  (kdz/leader-nested-map-init "search")
  (kdz/leader-nested-map-init "tab")
  (kdz/leader-nested-map-init "toggle")
  (kdz/leader-nested-map-init "toggle-global")
  (kdz/leader-nested-map-init "window")

  (kdz/leader-code-def "e"   `("Code Evaluation" . ,kdz-code-eval-actions-map))
  (kdz/leader-code-def "l"   `("Lookup Symbols"  . ,kdz-code-lookup-actions-map))
  (kdz/leader-toggle-def "g" `("Toggle (Global)" . ,kdz-toggle-global-actions-map))

  (kdz/leader-def
    ;; Top-level actions
    "SPC" '("Execute Command"          . execute-extended-command)
    "RET" '("Show Context Menu"        . context-menu-open)
    "u"   '("Apply Universal Argument" . universal-argument)
    ";"   '("Evaluate Expression"      . pp-eval-expression)

    ;; Sub-leader menus
    "b"    (cons "Buffer"          kdz-buffer-actions-map)
    "c"    (cons "Code"            kdz-code-actions-map)
    "e"    (cons "Edit"            kdz-edit-actions-map)
    "f"    (cons "File"            kdz-file-actions-map)
    "g"    (cons "Git"             kdz-git-actions-map)
    "h"    (cons "Help & Info"     kdz-help-actions-map)
    "i"    (cons "Insert"          kdz-insert-actions-map)
    "o"    (cons "Open"            kdz-open-actions-map)
    "s"    (cons "Search"          kdz-search-actions-map)
    "t"    (cons "Toggle"          kdz-toggle-actions-map)
    "w"    (cons "Window"          kdz-window-actions-map)
    "TAB"  (cons "Workspace"       kdz-tab-actions-map)))

;; Make `elpaca' wait for `general' package to load so we can use the `:general'
;; keyword in future `use-package' delcarations.
(elpaca-wait)

(provide 'packages.d/bootstrap)
