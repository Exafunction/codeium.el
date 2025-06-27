<p align="center">
<img width="300" alt="Codeium" src="codeium.svg"/>
</p>

---

[![Discord](https://img.shields.io/discord/1027685395649015980?label=community&color=5865F2&logo=discord&logoColor=FFFFFF)](https://discord.gg/3XFf78nAx5)
[![Twitter Follow](https://img.shields.io/badge/style--blue?style=social&logo=twitter&label=Follow%20%40codeiumdev)](https://twitter.com/intent/follow?screen_name=codeiumdev)
![License](https://img.shields.io/github/license/Exafunction/codeium.vim)
[![Docs](https://img.shields.io/badge/Codeium%20Docs-09B6A2)](https://docs.codeium.com)
[![Canny Board](https://img.shields.io/badge/Feature%20Requests-6b69ff)](https://codeium.canny.io/feature-requests/)
[![built with Codeium](https://codeium.com/badges/main)](https://codeium.com?repo_name=exafunction%2Fcodeium.el)

[![Visual Studio](https://img.shields.io/visual-studio-marketplace/i/Codeium.codeium?label=Visual%20Studio&logo=visualstudio)](https://marketplace.visualstudio.com/items?itemName=Codeium.codeium)
[![JetBrains](https://img.shields.io/jetbrains/plugin/d/20540?label=JetBrains)](https://plugins.jetbrains.com/plugin/20540-codeium/)
[![Open VSX](https://img.shields.io/open-vsx/dt/Codeium/codeium?label=Open%20VSX)](https://open-vsx.org/extension/Codeium/codeium)
[![Google Chrome](https://img.shields.io/chrome-web-store/users/hobjkcpmjhlegmobgonaagepfckjkceh?label=Google%20Chrome&logo=googlechrome&logoColor=FFFFFF)](https://chrome.google.com/webstore/detail/codeium/hobjkcpmjhlegmobgonaagepfckjkceh)

# codeium.el

_Free, ultrafast, extensible AI code completion tool for Emacs_

Codeium autocompletes your code with AI in all major IDEs. We [launched](https://www.codeium.com/blog/codeium-copilot-alternative-in-emacs) this implementation of the Codeium plugin for Emacs to bring this modern coding superpower to more developers. Check out our [playground](https://www.codeium.com/playground) if you want to quickly try out Codeium online.

codeium.el provides a `completion-at-point-functions` backend. It is designed to be use with a front-end, such as [company-mode](https://company-mode.github.io/), [corfu](https://github.com/minad/corfu), or the built-in `completion-at-point`.

codeium.el is an open source client and (mostly) written by [Alan Chen](https://github.com/Alan-Chen99). It uses a proprietary language server binary, currently downloaded (automatically, with confirmation) from [here](https://github.com/Exafunction/codeium/releases/). Use `M-x codeium-diagnose` to see apis/fields that would be sent to the local language server, and the command used to run the local language server. Customize `codeium-api-enabled`, `codeium-fields-regexps` and `codeium-command` to change them.

Contributions are welcome! Feel free to submit pull requests and issues related to the package.

<br />

![Emacs Demo - Final](https://user-images.githubusercontent.com/7545794/219270660-f14ddb8c-7087-4d97-9a15-b043467a2c00.gif)

<br />

## 🚀 Getting started

1. Install [Emacs](https://www.gnu.org/software/emacs/), ensuring the version of Emacs you are running is compiled with [libxml2](https://www.gnu.org/software/emacs/manual/html_node/elisp/Parsing-HTML_002fXML.html). You can check this by using the `(libxml-available-p)` function within Emacs Lisp. This function returns t (true) if libxml2 is available in your current Emacs session.

2. Install a text-completion frontend of your choice. (We recommend [company-mode](https://company-mode.github.io/) or [corfu](https://github.com/minad/corfu)).

3. Install `Exafunction/codeium.el` using your emacs package manager of
choice, or manually. See [Installation Options](#-installation-options) below.

4. Run `M-x codeium-install` to set up the package.

5. Add `codeium-completion-at-point` to your `completion-at-point-functions`.

6. Start seeing suggestions!

## 🛠️ Configuration

You can see all customization options via `M-x customize`.
(better documentation coming soon!)

Here is an example configuration:
```elisp
;; we recommend using use-package to organize your init.el
(use-package codeium
    ;; if you use straight
    ;; :straight '(:type git :host github :repo "Exafunction/codeium.el")
    ;; otherwise, make sure that the codeium.el file is on load-path

    :init
    ;; use globally
    (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
    ;; or on a hook
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

    ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions
    ;;             (list (cape-capf-super #'codeium-completion-at-point #'lsp-completion-at-point)))))
    ;; an async company-backend is coming soon!

    ;; codeium-completion-at-point is autoloaded, but you can
    ;; optionally set a timer, which might speed up things as the
    ;; codeium local language server takes ~0.2s to start up
    ;; (add-hook 'emacs-startup-hook
    ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

    ;; :defer t ;; lazy loading, if you want
    :config
    (setq use-dialog-box nil) ;; do not use popup boxes

    ;; if you don't want to use customize to save the api-key
    ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

    ;; get codeium status in the modeline
    (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
    (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
    ;; alternatively for a more extensive mode-line
    ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

    ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
    (setq codeium-api-enabled
        (lambda (api)
            (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
    ;; you can also set a config for a single buffer like this:
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local codeium/editor_options/tab_size 4)))

    ;; You can overwrite all the codeium configs!
    ;; for example, we recommend limiting the string sent to codeium for better performance
    (defun my-codeium/document/text ()
        (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
    ;; if you change the text, you should also change the cursor_offset
    ;; warning: this is measured by UTF-8 encoded bytes
    (defun my-codeium/document/cursor_offset ()
        (codeium-utf8-byte-length
            (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
    (setq codeium/document/text 'my-codeium/document/text)
    (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))
```


Here is an example configuration for company-mode.
```elisp
(use-package company
    :defer 0.1
    :config
    (global-company-mode t)
    (setq-default
        company-idle-delay 0.05
        company-require-match nil
        company-minimum-prefix-length 0

        ;; get only preview
        company-frontends '(company-preview-frontend)
        ;; also get a drop down
        ;; company-frontends '(company-pseudo-tooltip-frontend company-preview-frontend)
        ))
```

You can also access codeium.el from elisp; here is a snippet that returns
the full response of a `GetCompletions` request:
```elisp
(cl-letf*
    (
        ;; making a new codeium-state (thus a new local language server process)
        ;; takes ~0.2 seconds; avoid when possible
        (state (codeium-state-make :name "example"))
        ((codeium-config 'codeium/document/text state) "def fibi(n):")
        ((codeium-config 'codeium/document/cursor_offset state) 12)
        ((codeium-config 'codeium-api-enabled state) (lambda (api) (eq api 'GetCompletions))))
    (unwind-protect
        (progn
            (codeium-init state)
            ;; make async requests using codeium-request
            (cdr (codeium-request-synchronously 'GetCompletions state nil)))
        ;; cleans up temp files, kill process. Scheduled async requests on this state will be dropped.
        (codeium-reset state)))
```
Note that, among other things, you get probabilities for each token!
We would love to see a PR or your own package that uses those!

### 🔓 Authentication
If you want to authenticate automatically, add your codeium api key to one of `auth-sources`. For example

~/.authinfo.gpg:
``` text
machine codeium.com login apikey secret <insert_api_key_here>
```

## 💾 Installation Options

### ➡️ straight.el

```elisp
(straight-use-package '(codeium :type git :host github :repo "Exafunction/codeium.el"))
```

### 💀 Doom Emacs
In `packages.el` add the following:
```elisp
(package! codeium :recipe (:host github :repo "Exafunction/codeium.el"))
```
Add the example configuration to your `config.el` file.


### 💪 Manual

Run the following.

```bash
git clone --depth 1 https://github.com/Exafunction/codeium.el ~/.emacs.d/codeium.el
```

Add the following to your `~/.emacs.d/init.el` file.

```elisp
(add-to-list 'load-path "~/.emacs.d/codeium.el")
```

*Do you have a working installation for another Emacs environment (Spacemacs)? Submit a PR so we can share it with others!*

## Self-Hosted Enterprise

If you are using the plugin with the Windsurf Self-Hosted Enterprise deployment, you'll need to set your Portal and API URLs
in your vim config file so that Windsurf knows where to send completion requests. Add the following line to your `~/.emacs.d/init.el`:

```
(setq-default codeium-enterprise t)
(setq-default codeium-portal-url "<PORTAL URL>")
(setq-default codeium-api-url "<PORTAL URL>/_route/api_server")
```
