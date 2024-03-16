(require 'f)

(defun kdz/bin-available (bin)
  "Check if a BIN is available via `which'."
  (when (eq (call-process "which" nil nil nil bin) 0) t))

(defun kdz/get-brew-prefix ()
  "Get the path of `brew --prefix' for the system, assuming brew is present."
  (with-temp-buffer
    (if  (eq (call-process "brew" nil (current-buffer) nil "--prefix") 0)
        (string-trim (buffer-string))
      (error "Failed to get brew prefix path!"))))

(defun kdz/brew-prefix ()
  "Get the path of `brew --prefix' for the system."
  (if (kdz/bin-available "brew")
      (kdz/get-brew-prefix)
    (error "Homebrew is not available")))

(defun brew-bin (bin)
  "Given a BIN, generate the path for this bin assuming the homebrew prefix"
  (f-join (kdz/brew-prefix) "bin" bin))

(defun kdz/asdf-which (bin)
  (with-temp-buffer
    (let ((asdf-lookup
           (list (call-process "asdf" nil (current-buffer) nil "which" bin)
                 (substring (buffer-string) 0 -1))))
      (if (eq 0 (pop asdf-lookup)) (pop asdf-lookup)))))

(defun kdz/config-resource (name)
  (expand-file-name name (expand-file-name "resources" doom-user-dir)))
