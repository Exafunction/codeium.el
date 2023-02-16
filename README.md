<p align="center">
  <img width="300" alt="Codeium" src="codeium.svg"/>
</p>

---

[![Discord](https://img.shields.io/discord/1027685395649015980?label=community&color=5865F2&logo=discord&logoColor=FFFFFF)](https://discord.gg/3XFf78nAx5)
[![Twitter Follow](https://img.shields.io/badge/style--blue?style=social&logo=twitter&label=Follow%20%40codeiumdev)](https://twitter.com/intent/follow?screen_name=codeiumdev)
![License](https://img.shields.io/github/license/Exafunction/codeium.vim)

[![Visual Studio](https://img.shields.io/visual-studio-marketplace/i/Codeium.codeium?label=Visual%20Studio&logo=visualstudio)](https://marketplace.visualstudio.com/items?itemName=Codeium.codeium)
[![JetBrains](https://img.shields.io/jetbrains/plugin/d/20540?label=JetBrains)](https://plugins.jetbrains.com/plugin/20540-codeium/)
[![Open VSX](https://img.shields.io/open-vsx/dt/Codeium/codeium?label=Open%20VSX)](https://open-vsx.org/extension/Codeium/codeium)
[![Google Chrome](https://img.shields.io/chrome-web-store/users/hobjkcpmjhlegmobgonaagepfckjkceh?label=Google%20Chrome&logo=googlechrome&logoColor=FFFFFF)](https://chrome.google.com/webstore/detail/codeium/hobjkcpmjhlegmobgonaagepfckjkceh)

# codeium.el

_Free, ultrafast AI code completion tool for Emacs_

Codeium autocompletes your code with AI in all major IDEs. We launched this implementation of the Codeium package for Emacs to bring this modern coding superpower to more developers. Check out our [playground](https://www.codeium.com/playground) if you want to quickly try out Codeium online.

Contributions are welcome! Feel free to submit pull requests and issues related to the package.

<br />

![Emacs demo v4](https://user-images.githubusercontent.com/7545794/219252564-7ad530ee-ac72-4e29-9bec-c2bafb42efa8.gif)

<br />

## 🚀 Getting started

1. Install [Emacs](https://www.gnu.org/software/emacs/)

2. Install a text-completion frontend of your choice. (We recommend [company-mode](https://company-mode.github.io/)).

3. Install `Exafunction/codeium.el` using your emacs package manager of
   choice, or manually. See [Installation Options](#-installation-options) below.

4. Run `M-x codeium-install` to set up the package.

5. Add `codeium-completion-at-point` to your `completion-at-point-functions`.

6. Start seeing suggestions!

Here's an example installation using `straight.el`.

```elisp
(use-package codeium
    :straight '(:type git :host github :repo "Exafunction/codeium.el")
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

## 🛠️ Configuration

You can see customization options via `M-x customize`.

Here is a recommended configuration for company-mode.

```elisp
(setq-default
    company-idle-delay 0
    company-require-match nil
    company-minimum-prefix-length 0

    company-format-margin-function nil
    )
;; Make case-insensitive
(setq-default
    completion-ignore-case t
    company-dabbrev-code-ignore-case t
    company-dabbrev-ignore-case t
    company-etags-ignore-case t

    company-dabbrev-downcase nil
    )

;; always show tooltip, and never echo
(setq company-frontends
    '(
            company-preview-frontend
            ))

(global-company-mode)
(set-face-attribute 'company-preview nil :foreground "gray" :background "black" :slant 'oblique)
(set-face-attribute 'company-preview-common nil :foreground "gray" :background "black" :slant 'oblique)
```

## 💾 Installation Options

### ➡️ straight.el

```elisp
(use-package codeium
    :straight '(:type git :host github :repo "Exafunction/codeium.el")
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

### 💪 Manual

Run the following.

```bash
git clone https://github.com/Exafunction/codeium.el ~/.emacs.d/codeium.el
```

Add the following to your `~/.emacs.d/init.el` file.

```elisp
(add-to-list 'load-path (concat user-emacs-directory "codeium.el/" ))

(add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
;; or on a hook
(add-hook 'text-mode-hook
    (lambda ()
        (setq-local completion-at-point-functions '(codeium-completion-at-point))))
```
