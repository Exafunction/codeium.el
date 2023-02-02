# codeium.el

```elisp
(use-package codeium
    :straight '(:type git :host github :repo "Alan-Chen99/codeium.el")
    :ensure t
    :init
    ;; use globally
    (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
    ;; or on a hook
    (add-hook 'text-mode-hook
        (lambda ()
            (setq-local completion-at-point-functions '(codeium-completion-at-point))))
    :defer t
    :config
    ;; where you installed your executable
    ;; currently you can install from https://github.com/Exafunction/codeium/releases/
    (setq codeium-executable-loc (expand-file-name "codeium/codeium_language_server" user-emacs-directory)))
```
