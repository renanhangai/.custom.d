;;
;; Custom configuration for emacs
;;
(setq my-package-list
      '(ecb
		emmet-mode
		flycheck
		json-mode
		web-mode
		xclip ))
(setq-default tab-width 4)
(setq-default tab-stop-list (number-sequence tab-width 200 tab-width))
(setq-default indent-tabs-mode t)
(setq-default c-basic-indent tab-width)
(setq-default c-basic-offset tab-width)

;;=======================================
;; Install default packages
;;=======================================
(defun my-package-initialize ()
  (require 'package)
  (add-to-list 'package-archives
			   '("melpa" . "https://melpa.org/packages/") t))
(my-package-initialize)

(defun user-configure()
  ;; Install packages
  (package-initialize t)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package my-package-list)
    (unless (package-installed-p package)
      (package-install package)))

  ;; IDO
  (require 'ido)
  (ido-mode t)
  
  ;; Enable line numbers
  (require 'linum)
  (global-linum-mode)

  ;; Enable mouse usage on terminal mode
  (unless window-system
	(require 'mouse)
	(xterm-mouse-mode t))

  ;; Enable xclip on terminal mode
  (unless window-system
	(require 'xclip)
	(xclip-mode t))
  

  ;;
  ;; Auto mode list configuration
  ;;
  ;; Configure extensions to load
  ;;
  (defun user-configure-file-modes ()
	(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
	(add-to-list 'auto-mode-alist '("\\.babelrc\\'" . json-mode))
	(add-to-list 'auto-mode-alist '("\\.eslintrc\\'" . json-mode))
	(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
	)
  (user-configure-file-modes)
  )
(add-hook 'after-init-hook 'user-configure)

;;====================================
;; Configure specific modes
;;====================================

;; web-mode
(add-hook
 'web-mode-hook
 (lambda()
   (require 'emmet-mode)
   (emmet-mode)

   ; Enable eslint
   (require 'flycheck)
   (flycheck-mode)
   (flycheck-add-mode 'javascript-eslint 'web-mode)

   (web-mode-use-tabs)
   (setq web-mode-markup-indent-offset tab-width)
   (setq web-mode-css-indent-offset tab-width)
   (setq web-mode-code-indent-offset tab-width)
   (setq web-mode-script-padding tab-width)
   (setq web-mode-style-padding tab-width)))
