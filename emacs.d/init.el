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
(global-linum-mode t)
(setq fill-column 100)
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 110
                    :weight 'semi-bold)

 (setq org-log-done 'time)
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
                 evil-leader
                 markdown-mode
                 anzu
                 powerline
                 powerline-evil
                 rainbow-delimiters
                 nyan-mode
                 neotree
                 smartparens
                 helm
                 auto-package-update
                 rust-mode
                 rustfmt
                 racer
                 company
                 monokai-theme
                 seq
                 flycheck
                 flycheck-rust
                 fill-column-indicator
                 yasnippet))

; Update Package weekly ## Commented out this weekly update due to unnecessary updating of plugins
; (require 'auto-package-update)
; (setq auto-package-update-interval 30)
; (auto-package-update-maybe)

; Helm Settings
(require 'helm-config)
(helm-mode 1)

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

; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
 (add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;;Language Specific Settings

; Rust
(require 'rust)

; OCaml
;; Tureg
(load "/home/cry0g3n/.opam/system/share/emacs/site-lisp/tuareg-site-file")
;; Caml-mode
(add-to-list 'load-path "/home/cry0g3n/.opam/system/share/emacs/site-lisp/")


; Golang

;; godoc
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;; GOPATH
(setenv "GOPATH" "/home/cry0g3n/Applications/go")

;; Go-mode-setup
(defun go-mode-setup ()
 (setq compile-command "go build -v; and go test -v; and go vet")
 (define-key (current-local-map) "\C-c\C-c" 'compile)
 (go-eldoc-setup)
 (setq gofmt-command "goimports")
 (add-hook 'before-save-hook 'gofmt-before-save)
 (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'go-mode-setup)

;; Go-Autocomplete
(require 'auto-complete-config)
(require 'go-autocomplete)
