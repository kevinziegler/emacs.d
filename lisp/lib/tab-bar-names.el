;;; -*- lexical-binding: t -*-

(defun kdz/tab-bar-tab-name-format (tab i)
  (let* ((tab-face   (funcall tab-bar-tab-face-function tab)))
    (s-replace "  " " "
               (format (propertize  "[ %d %s %s%s ]" 'face tab-face)
                       i
                       (kdz/tab-bar-tab-icon-segment tab tab-face)
                       (kdz/tab-bar-tab-name-segment tab tab-face)
                       (kdz/tab-bar-tab-git-segment  tab tab-face)))))

(defun kdz/tab-bar-tab-icon-segment (tab tab-face)
  (if-let* ((tab-name (alist-get 'name tab))
            (icon (cdr (assoc tab-name kdz-tab-bar-tab-icons))))
      (kdz/propertize-nerd-icon icon `(face (:inherit ,tab-face)))
    ""))

(defun kdz/tab-bar-tab-name-segment (tab tab-face)
  (if (and (not (eq (car tab) 'current-tab)) (kdz/tab-bar-system-tab-p tab))
      ""
    (alist-get 'name tab)))

(defun kdz/git-repo-name-for (dir)
  (when-let* ((file-directory-p (expand-file-name ".git" dir))
              (remote-url
               (shell-command-to-string
                (format "git -C %s config --get remote.origin.url " dir))))
    (f-base remote-url)))

(defun kdz/git-branch-name-for (dir)
  (when (file-directory-p (expand-file-name ".git" dir))
    (s-trim (shell-command-to-string (format "git -C %s rev-parse --abbrev-ref HEAD"
                                             dir)))))

(defun kdz/otpp-tab-git-branch (tab)
  (when-let* ((tab-root (alist-get 'otpp-root-dir tab)))
    (kdz/git-branch-name-for tab-root)))

(defun kdz/otpp-tab-git-repo (tab)
  (when-let* ((tab-root (alist-get 'otpp-root-dir tab)))
    (kdz/git-branch-name-for tab-root)))

(defun kdz/tab-bar-tab-git-segment (tab tab-face)
  ""
  ;; (if-let ((branch-name (kdz/otpp-tab-git-branch tab)))
  ;;     (concat ":" (propertize branch-name 'face 'tab-bar))
  ;;   "")
  )

(provide 'lib/tab-bar-names)
