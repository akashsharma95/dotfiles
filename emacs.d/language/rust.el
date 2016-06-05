;; Set path to racer binary
(setq racer-cmd "/home/cry0g3n/.cargo/bin/racer")

;; Set path to rust src directory
(setq racer-rust-src-path "/home/cry0g3n/Documents/Projects/rust/src/")

;; Load rust-mode when you open `.rs` files
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; Setting up configurations when you load rust-mode
(add-hook 'rust-mode-hook
     '(lambda ()
         ;; Enable racer
         (racer-mode)

         ;; Hook in racer with eldoc to provide documentation
         (add-hook 'racer-mode-hook #'eldoc-mode)

         ;; Fill Column Indicator
        (fci-mode 1)
        (setq  fci-rule-column 80)

         ;; Use flycheck-rust in rust-mode
         (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

         ;; Key binding to auto complete and indent
         (global-set-key (kbd "TAB") #'company-indent-or-complete-common) ;
         (setq company-tooltip-align-annotations t))

         ;; Goto definition
         (global-set-key (kbd "\C-c j") 'racer-find-definition)
)
(provide 'rust)
