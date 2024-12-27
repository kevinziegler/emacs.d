(require 'f)

(defun kdz/brew-prefix ()
  "Get the path of `brew --prefix' for the system."
  (if (executable-find "brew")
      (with-temp-buffer
        (if  (eq (call-process "brew" nil (current-buffer) nil "--prefix") 0)
            (string-trim (buffer-string))
          (error "Failed to get brew prefix path!")))
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

(provide 'lib/system)
