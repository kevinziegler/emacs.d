(setq package-enable-at-startup nil
      byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp) (bytecomp))
      native-comp-async-report-warnings-errors 'silent
      gc-cons-threshold 10000000
      inhibit-splash-screen t
      startup-screen-inhibit-startup-screen t
      frame-resize-pixelwise t)

(setenv "LSP_USE_PLISTS" "true")

(add-to-list 'default-frame-alist '(height . 70))
(add-to-list 'default-frame-alist '(width . 235))
(add-to-list 'default-frame-alist '(undecorated-round . t))
(add-to-list 'default-frame-alist '(internal-border-width . 3))
(add-to-list 'default-frame-alist '(vertical-border-width . 3))

(scroll-bar-mode -1)
(tool-bar-mode -1)

(defun kdz/user-directory (&rest parts)
  "Return the path represented by PARTS under `user-emacs-directory'"
  (seq-reduce (lambda (base part) (expand-file-name part base))
              parts
              user-emacs-directory))

(startup-redirect-eln-cache (kdz/user-directory ".local/eln-cache"))
