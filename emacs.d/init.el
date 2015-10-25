(add-to-list 'load-path (expand-file-name "language" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))

; Essential Settings
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(global-visual-line-mode nil)
(setq-default left-fringe-width nil)
(setq-default indent-tabs-mode nil)
(eval-after-load "vc" '(setq vc-handled-backends nil))
(setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)
(setq split-width-threshold nil)
(setq visible-bell t)
(setq fill-column 100)
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 110
                    :weight 'semi-bold)


; Package Settings
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)
(setq package-enable-at-startup nil)
(defun inverse-filter (condp lst)
  "A filter function, but returns a list of the entries that
don't match the predicate."
  (delq nil
        (mapcar (lambda (x) (and (not (funcall condp x)) x)) lst)))
(defun packages-install (packages)
  "Given a list of packages, this will install them from the standard locations."
  (let ((to-install (inverse-filter 'package-installed-p packages)))
    (when to-install
      (package-refresh-contents)
      (dolist (it to-install)
          (package-install it)
      (delete-other-windows)))))
(packages-install
               '(magit
                 evil
                 markdown-mode
                 anzu
                 powerline
                 powerline-evil
                 rainbow-delimiters
                 nyan-mode
                 smartparens
                 helm
                 auto-package-update
                 rust-mode
                 rustfmt
                 racer
                 company
                 flycheck
                 flycheck-rust
                 monokai-theme
                 fill-column-indicator
                 yasnippet))

; Update Package weekly
(require 'auto-package-update)
(setq auto-package-update-interval 7)
(auto-package-update-maybe)

; Helm Settings
(require 'helm-config)

; Anzu-mode Settings
(global-anzu-mode +1)

; Powerline Settings
(require 'powerline)
(require 'powerline-evil)
(require 'powerline-theme)
( powerline-spacemacs-imitation-theme )


; Rainbow-Delimiters Settings
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

; Fill Column Indicator
(require 'fill-column-indicator)

; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

; Evil-Mode Settings
(require 'evil)
(evil-mode t)
(load-theme 'monokai t)
; (load-theme 'material t)

; Smartparens
(require 'smartparens-config)
(smartparens-global-mode t)

; Company-Mode Settings
(global-company-mode)
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)

; YASnipppet
(require 'yasnippet)
(yas-global-mode 1)
;;Language Specific Settings

; Rust
(require 'rust)
