;;
;; Custom configuration for emacs
;;
(defvar my-tab-width 4 "Tab width configuration")
(defvar my-package-list
  '(emmet-mode
	flycheck
	json-mode
	js2-mode
	php-mode
	sass-mode
	scss-mode
	web-mode
	xclip )
  "Package list to be auto installed")
(defvar my-font-list
  '(
	("Source Code Pro" 100)
	)
  "Fonts to check")

;; Emacs tab identation
(setq-default tab-width my-tab-width)
(setq-default tab-stop-list (number-sequence my-tab-width 200 my-tab-width))
(setq-default indent-tabs-mode t)
(setq-default c-basic-indent my-tab-width)
(setq-default c-basic-offset my-tab-width)

;;=======================================
;; Initialize packages
;;=======================================
(defun my-package-initialize ()
  (require 'package)
  (add-to-list 'package-archives
			   '("melpa" . "https://melpa.org/packages/") t))
(my-package-initialize)

;;=======================================
;; User configuration
;;=======================================
(defun user-configure()
  ;; Install packages
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package my-package-list)
    (unless (package-installed-p package)
      (package-install package)))

  ;; Set the font from the list
  (defun my-get-default-font-spec ()
	"Get the default font"
    (let ((font-spec nil)
		  (current-font-spec nil)
		  (current-font-face nil)
		  (i 0)
		  (len (length my-font-list)))
	  (while (and (< i len) (eq font-spec nil))
		(setq current-font-spec (nth i my-font-list))
		(setq current-font-face (nth 0 current-font-spec))
		(when (member current-font-face (font-family-list))
		  (setq font-spec current-font-spec)))
	  font-spec))
  (when window-system
    (let ((font-spec (my-get-default-font-spec)))
      (when font-spec
		(let ((font-spec-face (nth 0 font-spec))
			  (font-spec-size (nth 1 font-spec)))
		  (set-face-attribute 'default nil :height font-spec-size :family font-spec-face)))))
  
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (when window-system
	(load-theme 'wombat))

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
	(add-to-list 'auto-mode-alist '("\\.babelrc\\'" . json-mode))
	(add-to-list 'auto-mode-alist '("\\.eslintrc\\'" . json-mode))
	(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
	(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
	(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
	)
  (user-configure-file-modes)
  )
(add-hook 'after-init-hook 'user-configure)

;;====================================
;; Configure specific modes
;;====================================
;; c++-mode
(add-hook
 'c++-mode-hook
 (lambda ()
   (c-add-style
    "my-cc-mode"
    '("cc-mode"
      (c-offsets-alist
	   .
	   ((innamespace . 0)
		(arglist-close . 0)
		(inline-open . 0)
		(substatement-open . 0)
		(case-label . +)
		(inher-intro . 0)
		))))
   (c-set-style "my-cc-mode")
   (setq c-default-style "linux")
   (setq indent-tabs-mode t)
   (setq c-basic-offset my-tab-width)
   (setq c-basic-indent my-tab-width)   
   (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)))

;; js2-mode
(add-hook
 'js2-mode-hook
 (lambda()
   (require 'flycheck)
   (flycheck-mode)
   (add-to-list 'flycheck-eslint-rules-directories "/home/rhangai/.emacs.d")
   (setq js2-strict-trailing-comma-warning nil)))

;; php-mode
(add-hook
 'php-mode-hook
 (lambda()
   (require 'flycheck)
   (flycheck-mode)
   (setq indent-tabs-mode t
		 tab-width my-tab-width)))

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
   (setq web-mode-markup-indent-offset my-tab-width)
   (setq web-mode-css-indent-offset my-tab-width)
   (setq web-mode-code-indent-offset my-tab-width)
   (setq web-mode-script-padding my-tab-width)
   (setq web-mode-style-padding my-tab-width)))


