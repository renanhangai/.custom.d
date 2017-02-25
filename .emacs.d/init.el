;;; Load setup emacs
(let* ((this-file-dir (file-name-directory load-file-name))
      (setup-file (concat this-file-dir "setup.el")))
  (when (file-readable-p setup-file)
    (load-file setup-file)))


;; Custom configuration script
