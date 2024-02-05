(defvar bootstrap-version)
(setq straight-base-dir (kdz/user-directory ".local"))

(let ((bootstrap-file (kdz/user-directory ".local"
                                          "straight"
                                          "repos"
                                          "straight.el"
                                          "bootstrap.el"))
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
(setq straight-use-package-by-default t)
