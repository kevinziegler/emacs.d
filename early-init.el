;;; -*- lexical-binding: t -*-
(setq package-enable-at-startup nil
      byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp) (bytecomp))
      native-comp-async-report-warnings-errors 'silent
      gc-cons-threshold 10000000
      inhibit-splash-screen t
      startup-screen-inhibit-startup-screen t
      frame-resize-pixelwise t)

(setenv "LSP_USE_PLISTS" "true")

(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Also set frame parameters (extra safety for the very first frame)
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

(add-to-list 'default-frame-alist '(height . 70))
(add-to-list 'default-frame-alist '(width . 235))
(add-to-list 'default-frame-alist '(undecorated-round . t))
(add-to-list 'default-frame-alist '(internal-border-width . 3))
(add-to-list 'default-frame-alist '(vertical-border-width . 3))

(defun kdz/user-directory (&rest parts)
  "Return the path represented by PARTS under `user-emacs-directory'"
  (seq-reduce (lambda (base part) (expand-file-name part base))
              parts
              user-emacs-directory))

;; Even with the variable above, some Emacs builds still push the
;; native-compiler warnings into the regular *Warnings* buffer via
;; `display-warning'. Add (native-compiler) to the suppression lists so
;; the popup never appears and the log itself is muted.
(with-eval-after-load 'warnings
  (setq warning-suppress-types
        (append warning-suppress-types
                '((native-compiler)
                  (comp))))
  (setq warning-suppress-log-types
        (append warning-suppress-log-types
                '((native-compiler)
                  (comp)))))

(startup-redirect-eln-cache (kdz/user-directory ".local/eln-cache"))
