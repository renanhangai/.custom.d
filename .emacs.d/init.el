;;; Load setup emacs
(let* ((this-file-dir (file-name-directory load-file-name))
      (setup-file (concat this-file-dir "setup.el")))
  (when (file-readable-p setup-file)
    (load-file setup-file)))


;; Custom configuration script
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mmm-default-submode-face ((t nil))))
