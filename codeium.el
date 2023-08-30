;;; codeium.el --- codeium client for emacs         -*- lexical-binding: t; -*-

;; MIT License

;; Copyright (c) 2023 Exafunction

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; use M-x `codeium-install' to install binaries automatically
;; add `codeium-completion-at-point' to your `completion-at-point-functions'
;; use `codeium-diagnose' to see currently enabled apis and fields

;; anything defined by `codeium-def' a constant or a function that
;; takes 1, 2, or 3 arguments, which are (api state val)
;; api is a symbol such as 'GetCompletions, state is of type `codeium-state'
;; which keeps all the state (including a process, port, and some hash tables)
;; val is only relevant if the field is only sensible given a previous
;; value, such as `codeium/request_id' used in `'CancelRequest'

;; use M-x `customize' see a full list of settings.

;;; Code:

(defvar codeium-latest-local-server-version "1.2.78")

;; (require 'url-parse)
(autoload 'url-parse-make-urlobj "url-parse")

(eval-when-compile
	(require 'url-vars)
	(defvar url-http-end-of-headers)
	(defvar url-http-response-status))

(defgroup codeium nil
	"codeium.el customization -some-doc-str-here-"
	:group 'convenience)
(defvar codeium-log-waiting-text (propertize "waiting for response" 'face '(:weight ultra-bold)))

(defvar codeium-fullpath-alist nil)

(eval-and-compile
	(defun codeium-default-func-name (symbol-name)
		(if (string-prefix-p "codeium" symbol-name)
			(concat "codeium-default" (substring symbol-name (length "codeium")))
			(error "invalid name"))))

(eval-and-compile
	(defun codeium-def-handle-args (args)
		(let ((doc-str nil) (value nil) (arglist nil) (body nil))
			(ignore doc-str value arglist body)
			(pcase-let*
				(
					(
						(or
							`(,value)
							`(,value ,(and (pred stringp) doc-str))
							`(,arglist ,(and (pred stringp) doc-str) . ,body)
							`(,arglist . ,body))
						args))
				(list doc-str value arglist body)))))

(defmacro codeium-def (name &rest args)
	(declare (doc-string 3))
	(pcase-let*
		(
			(`(,doc-str ,value ,arglist ,body) (codeium-def-handle-args args))
			(funcsymbol (when body (intern (codeium-default-func-name (symbol-name name)))))
			(value (or value `',funcsymbol))

			(fullpath
				(when (string-prefix-p "codeium/" (symbol-name name))
					(mapcar #'intern (cdr (split-string (symbol-name name) "/")))))
			(funcdefform (when body `((defun ,funcsymbol ,arglist ,@body))))

			(doc-str (or doc-str ""))
			)
		`(progn
			 (setf (alist-get ',name codeium-fullpath-alist) ',fullpath)
			 (defcustom ,name ,value
				 ;; i can probably process the doc-str
				 ,doc-str
				 :type 'sexp
				 :group 'codeium)
			 ,@funcdefform)))

(codeium-def codeium-delay 0.001)

(codeium-def codeium-directory (_api state) (codeium-state-manager-directory state))
(codeium-def codeium-port (_api state) (codeium-state-port state))

(defun codeium-get-language-server-string ()
	(let ((arch
			  (unless (eq system-type 'windows-nt)
				  (if (string= (string-trim (shell-command-to-string "uname -m")) "x86_64")
					  "x64" "arm"))))
		(pcase system-type
			('windows-nt "language_server_windows_x64.exe")
			('gnu/linux (concat "language_server_linux_" arch))
			('darwin (concat "language_server_macos_" arch))
			(_ (error "unable to automatically determine your system, or your system is not supported yet. Please file an issue on github.")))))

(codeium-def codeium-local-server-version codeium-latest-local-server-version)

(codeium-def codeium-download-url
	(condition-case err;; don't signal error on loading
		(concat "https://github.com/Exafunction/codeium/releases/download/language-server-v"
			codeium-local-server-version "/" (codeium-get-language-server-string) ".gz")
		(error
			(defvar codeium-download-url (lambda () (signal (car err) (cdr err))))
			nil)))

(defconst codeium-apis
	'(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))

(codeium-def codeium-api-enabled () t)

(codeium-def codeium-fields-regexps
	`(
		 (GetCompletions .
			 ,(rx bol "codeium/" (or "metadata" "document" "editor_options") "/" (* anychar) eol))
		 (Heartbeat .
			 ,(rx bol "codeium/metadata/" (* anychar) eol))
		 (CancelRequest .
			 ,(rx bol "codeium/" (or (seq "metadata/" (* anychar)) "request_id")  eol))
		 (GetAuthToken)
		 (RegisterUser .
			 ,(rx bol "codeium/firebase_id_token" eol))
		 (AcceptCompletion .
			 ,(rx bol "codeium/" (or (seq "metadata/" (* anychar)) "completion_id")  eol))
		 ))
(codeium-def codeium-api-fields (api)
	(let ((regexp (alist-get api codeium-fields-regexps)))
		(if (stringp regexp)
			(remq nil
				(mapcar
					(lambda (el)
						(when (string-match regexp (symbol-name (car el)))
							(car el)))
					codeium-fullpath-alist))
			nil)))

(defvar codeium-special-url-alist '((auth-redirect . "/auth")))
(codeium-def codeium-url (api state)
	(let ((endpoint
			  (or (alist-get api codeium-special-url-alist)
				  (concat "/exa.language_server_pb.LanguageServerService/" (symbol-name api)))))
		(url-parse-make-urlobj "http" nil nil "localhost" (codeium-state-port state)
			endpoint nil nil t)))


(codeium-def codeium/metadata/ide_name "emacs")
(codeium-def codeium/metadata/extension_version codeium-local-server-version)
(codeium-def codeium/metadata/ide_version emacs-version)
;; (codeium-def codeium/metadata/request_id (api)
;; 	(when (eq api 'GetCompletions)
;; 		(random most-positive-fixnum)))

(defvar codeium-global-requestid-counter 0)
(codeium-def codeium/metadata/request_id (api)
	(when (eq api 'GetCompletions)
		(cl-incf codeium-global-requestid-counter)))

;; for CancelRequest
(codeium-def codeium/request_id (_api _state val) val)

;; for AcceptCompletion
(codeium-def codeium/completion_id (_api _state val) val)

;; alternative getting key from file?
;; TODO
;; (setq codeium/metadata/api_key 'codeium-default/metadata/api_key)
(defun codeium-get-saved-api-key ())
(codeium-def codeium/metadata/api_key (_api state)
	(if-let ((api-key (or (codeium-state-last-api-key state) (codeium-get-saved-api-key))))
		(setq codeium/metadata/api_key api-key)
		(setq codeium/metadata/api_key
			(lambda (_api state)
				(when-let ((api-key (codeium-state-last-api-key state)))
					(setq codeium/metadata/api_key api-key))))
		nil))


(codeium-def codeium/document/text ()
	(buffer-string))
(codeium-def codeium/document/cursor_offset ()
	(codeium-utf8-byte-length (buffer-substring-no-properties (point-min) (point))))

(codeium-def codeium/document/editor_language () (symbol-name major-mode))

(defvar codeium-language-alist
	'(
		 (nil . 0)
		 (c-mode . 1)
		 (c-ts-mode . 1)
		 (clojure-mode . 2)
		 (clojurec-mode . 2)
		 (clojurescript-mode . 2)
		 (coffee-mode . 3)
		 (cc-mode . 4)
		 (c++-mode . 4)
		 (c++-ts-mode . 4)
		 (csharp-mode . 5)
		 (csharp-tree-sitter-mode . 5)
		 (csharp-ts-mode . 5)
		 (css-mode . 6)
		 (css-ts-mode . 6)
		 (cuda-mode . 7)
		 (dockerfile-mode . 8)
		 (dockerfile-ts-mode . 8)
		 (go-dot-mod-mode . 9)
		 (go-mod-ts-mode . 9)
		 (go-mode . 9)
		 (go-ts-mode . 9)
		 (groovy-mode . 10)
		 (haskell-mode . 12)
		 (terraform-mode . 13)
		 (html-mode . 14)
		 (sgml-mode . 14)
		 (mhtml-mode . 14)
		 (java-mode . 16)
		 (java-ts-mode . 16)
		 (jdee-mode . 16)
		 (ecmascript-mode . 17)
		 (javascript-mode . 17)
		 (js-mode . 17)
		 (js2-mode . 17)
		 (js-ts-mode . 17)
		 (rjsx-mode . 17)
		 (json-mode . 18)
		 (json-ts-mode . 18)
		 (julia-mode . 19)
		 (ess-julia-mode . 19)
		 (kotlin-mode . 20)
		 (kotlin-ts-mode . 20)
		 (latex-mode . 21)
		 (less-mode . 22)
		 (less-css-mode . 22)
		 (lua-mode . 23)
		 (lsp--render-markdown . 25)
		 (markdown-mode . 25)
		 (gfm-mode . 25)
		 (objc-mode . 26)
		 (perl-mode . 28)
		 (cperl-mode . 28)
		 (php-mode . 29)
		 (text-mode . 30)
		 (python-mode . 33)
		 (python-ts-mode . 33)
		 (cython-mode . 33)
		 (ess-r-mode . 34)
		 (ruby-mode . 35)
		 (enh-ruby-mode . 35)
		 (ruby-ts-mode . 35)
		 (rust-mode . 36)
		 (rust-ts-mode . 36)
		 (rustic-mode . 36)
		 (sass-mode . 37)
		 (ssass-mode . 37)
		 (scala-mode . 38)
		 (scss-mode . 39)
		 (sh-mode . 40)
		 (ebuild-mode . 40)
		 (pkgbuild-mode . 40)
		 (sql-mode . 41)
		 (swift-mode . 43)
		 (tsx-mode . 44)
		 (tsx-ts-mode . 44)
		 (ts-mode . 45)
		 (typescript-mode . 45)
		 (typescript-ts-mode . 45)
		 (nxml-mode . 48)
		 (xml-mode . 48)
		 (yaml-mode . 50)
		 (yaml-ts-mode . 50)
		 (conf-toml-mode . 52)
		 (toml-ts-mode . 52)
		 (dart-mode . 53)
		 (caml-mode . 55)
		 (tuareg-mode . 55)
		 (cmake-mode . 56)
		 (cmake-ts-mode . 56)
		 (pascal-mode . 57)
		 (elixir-mode . 58)
		 (elixir-ts-mode . 58)
		 (heex-ts-mode . 58)
		 (fsharp-mode . 59)
		 (lisp-data-mode . 60)))

(codeium-def codeium/document/language ()
	(let ((mode major-mode))
		(while (not (alist-get mode codeium-language-alist))
			(setq mode (get mode 'derived-mode-parent)))
		(alist-get mode codeium-language-alist)))

(codeium-def codeium/document/line_ending "\n"
	"according to https://www.reddit.com/r/emacs/comments/5b7o9r/elisp_how_to_concat_newline_into_string_regarding/
	this can be always \\n")

(codeium-def codeium/editor_options/tab_size ()
	tab-width)
(codeium-def codeium/editor_options/insert_spaces ()
	(if indent-tabs-mode :false t))

(codeium-def codeium/firebase_id_token (_api state) (codeium-state-last-auth-token state))

;;;###autoload
(cl-defstruct
	(codeium-state
		(:constructor codeium-state-make)
		(:copier nil))
	(name "")
	(config nil
		:documentation "state-wise config, access it with `codeium-config'")
	(proc nil
		:documentation "created on a `codeium-init', not created if one specifies `codeium-port'")
	(manager-directory nil
		:documentation "directory which codeium local language server places temp files; created by `codeium-default-command'")
	(port nil
		:documentation "port used by codeium local language server; by default a random port is used.
If you set `codeium-port', it will be used instead and no process will be created")
	(port-ready-hook nil
		:documentation "hook called when the server is ready; use `codeium-on-port-ready' to add to it")

	(alive-tracker nil
		:documentation "a symbol, set to nil on a codeium-reset which ensures that requests on timers made before the request are dropped")

	last-auth-token
	last-api-key

	(last-request-id 0)

	;; hash tables for codeium-request-synchronously
	;; these has distinct elements
	(results-table (make-hash-table :test 'eql :weakness nil)) ; results that are ready
	(pending-table (make-hash-table :test 'eql :weakness nil)) ; requestid that we are waiting for
	)

(codeium-def codeium-command-executable
	(expand-file-name
		(pcase system-type
			('windows-nt "codeium_language_server.exe")
			(_ "codeium_language_server"))
		(expand-file-name "codeium" user-emacs-directory)))

(codeium-def codeium-enterprise nil)
(codeium-def codeium-portal-url "https://www.codeium.com")
(codeium-def codeium-api-url "https://server.codeium.com")
(codeium-def codeium-register-user-url ()
			 (if codeium-enterprise
				 (concat codeium-api-url "/exa.api_server_pb.ApiServerService/RegisterUser")
			   "https://api.codeium.com/register_user/"))

(codeium-def codeium-command (api state)
	(unless (codeium-state-manager-directory state)
		(setf (codeium-state-manager-directory state) (make-temp-file "codeium_" t)))
	`(,(codeium-get-config 'codeium-command-executable api state)
		 "--api_server_url" ,(codeium-get-config 'codeium-api-url api state)
		 "--manager_dir" ,(codeium-state-manager-directory state)
         "--register_user_url" ,(codeium-get-config 'codeium-register-user-url api state)
         ,@(if (codeium-get-config 'codeium-enterprise api state) '("--enterprise_mode"))
         "--portal_url" ,(codeium-get-config 'codeium-portal-url api state)))

(defvar codeium-state (codeium-state-make :name "default"))

;;;###autoload
(defun codeium-config (field &optional state)
	(setq state (or state codeium-state))
	(if (eq (alist-get field (codeium-state-config state) 'noexist) 'noexist)
		(symbol-value field)
		(alist-get field (codeium-state-config state))))
(defun codeium--set-config (val field &optional state)
	(setq state (or state codeium-state))
	(setf (alist-get field (codeium-state-config state)) val))

;;;###autoload
(gv-define-setter codeium-config (val field &optional state)
	`(codeium--set-config ,val ,field ,state))


(defun codeium-get-config (field api state &optional given-val)
	(let ((val (codeium-config field state)))
		(if (functionp val)
			(cl-case (cdr (func-arity val))
				(0 (funcall val))
				(1 (funcall val api))
				(2 (funcall val api state))
				(t (funcall val api state given-val)))
			val)))

(defun codeium-nested-alist-get-multi (body top &rest rest)
	(if rest
		(apply #'codeium-nested-alist-get-multi (alist-get top body) rest)
		(alist-get top body)))
(defun codeium-nested-alist-set-multi (body val top &rest rest)
	(let ((cur-alist body))
		(setf (alist-get top cur-alist)
			(if rest
				(apply #'codeium-nested-alist-set-multi (alist-get top cur-alist) val rest)
				val))
		cur-alist))
(defun codeium-nested-alist-get (body field)
	(let ((fullpath (alist-get field codeium-fullpath-alist)))
		(unless fullpath (error "field %s is set to path %s which is not valid" field fullpath))
		(apply #'codeium-nested-alist-get-multi body fullpath)))
(defun codeium--nested-alist-set (body field val)
	(let ((fullpath (alist-get field codeium-fullpath-alist)))
		(unless fullpath (error "field %s is set to path %s which is not valid" field fullpath))
		(apply #'codeium-nested-alist-set-multi body val fullpath)))
(gv-define-expander codeium-nested-alist-get
	(lambda (do body field)
		(gv-letplace (getter setter) body
			(macroexp-let2 nil field field
				(funcall do `(codeium-nested-alist-get ,getter ,field)
					(lambda (v)
						(macroexp-let2 nil v v
							`(progn
								 ,(funcall setter`(codeium--nested-alist-set ,getter ,field ,v))
								 ,v))))))))


(defun codeium-compute-configs (api state vals-alist)
	(let (ans)
		(mapc
			(lambda (field)
				(setf (alist-get field ans) (codeium-get-config field api state (alist-get field vals-alist))))
			(codeium-get-config 'codeium-api-fields api state))
		ans))

(defun codeium-diagnose (&optional state)
	(interactive)
	(setq state (or state codeium-state))
	(with-output-to-temp-buffer "*codeium-diagnose*"

		(with-current-buffer standard-output
			(insert "codeium state: ")
			(insert (propertize (codeium-state-name state) 'face '(:weight ultra-bold)))
			(terpri)
			(insert "command: ")
			(let ((command
					  (if (codeium-state-proc state)
						  (process-command (codeium-state-proc state))
						  (insert "[will be]")
						  (codeium-get-config 'codeium-command nil state))))
				(terpri)
				(insert
					(propertize (mapconcat #'shell-quote-argument command " ")
						'face '(:weight ultra-bold)))
				(terpri)))
		(terpri)
		(mapc
			(lambda (api)
				(if (not (codeium-get-config 'codeium-api-enabled api state))
					(progn
						(with-current-buffer standard-output
							(insert (propertize (symbol-name api) 'face '(:weight ultra-bold :strike-through t))))
						(terpri)
						(terpri))
					(with-current-buffer standard-output
						(insert (propertize (symbol-name api) 'face '(:weight ultra-bold))))
					(terpri)
					(princ (url-recreate-url (codeium-get-config 'codeium-url api state)))
					(terpri)
					(mapc
						(lambda (item)
							;; help-insert-xref-button
							(with-current-buffer standard-output
								(help-insert-xref-button (symbol-name (car item)) 'help-variable-def (car item))
								(insert (propertize "\t" 'display '(space :align-to 40))))
							(let*
								(
									(print-escape-newlines t) (print-length 100)
									(obj (cdr item))
									(obj (if (stringp obj)
											 (substring-no-properties obj 0 (length obj)) obj)))
								(cl-prin1 obj))
							(terpri))
						(codeium-compute-configs api state nil))
					(terpri)))
			codeium-apis)))


(defun codeium-make-body-for-api (api state vals-alist)
	(let (body tmp)
		(mapc
			(lambda (field)
				(setq tmp (codeium-get-config field api state (alist-get field vals-alist)))
				(when tmp
					(setf (codeium-nested-alist-get body field) tmp)))
			(codeium-get-config 'codeium-api-fields api state))
		body))

(codeium-def codeium-log-buffer ()
	(let ((buf (get-buffer "*codeium-log*")))
		(if buf buf
			(setq buf (generate-new-buffer "*codeium-log*"))
			(with-current-buffer buf
				(special-mode)
				(buffer-disable-undo))
			buf)))

(codeium-def codeium-mode-line-enable nil)
(codeium-def codeium-mode-line-keep-time 3)


;; https://nullprogram.com/blog/2010/05/11/
;; ID: 90aebf38-b33a-314b-1198-c9bffea2f2a2
(defun codeium-uuid-create ()
	"Return a newly generated UUID. This uses a simple hashing of variable data."
	(let ((s (md5 (format "%s%s%s%s%s%s%s%s%s%s"
					  (user-uid)
					  (emacs-pid)
					  (system-name)
					  (user-full-name)
					  user-mail-address
					  (current-time)
					  (emacs-uptime)
					  (garbage-collect)
					  (random)
					  (recent-keys)))))
		(format "%s-%s-3%s-%s-%s"
			(substring s 0 8)
			(substring s 8 12)
			(substring s 13 16)
			(substring s 16 20)
			(substring s 20 32))))

(defvar codeium-last-auth-url nil)

(defun codeium-make-auth-url (state &optional uuid)
	(let*
		(
			(uuid (or uuid (codeium-uuid-create)))
			(query-params
				(url-build-query-string
					`(
						 ("response_type" "token")
						 ("state" ,uuid)
						 ("scope" "openid profile email")
						 ;; ("redirect_uri" "vim-show-auth-token")
						 ("redirect_uri" ,(url-recreate-url (codeium-get-config 'codeium-url 'auth-redirect state)))
						 ("redirect_parameters_type" "query"))))
			(url
             (concat (codeium-get-config 'codeium-portal-url 'auth-redirect state) "/profile?" query-params)))
		(setq codeium-last-auth-url url)))
(defun codeium-kill-last-auth-url ()
	(interactive)
	(when codeium-last-auth-url
		(message "%s sent to kill-ring" codeium-last-auth-url)
		(kill-new codeium-last-auth-url)))

(defun codeium-defer-until-no-input (state tracker func &optional args)
	(when (eq tracker (codeium-state-alive-tracker state))
		(if (input-pending-p)
			(run-with-idle-timer 0.005 nil #'codeium-defer-until-no-input state tracker func args)
			(with-local-quit
				(apply func args)))))
(defun codeium-run-with-timer (state secs func &rest args)
	(unless (codeium-state-alive-tracker state)
		(error "codeium-state is not alive! %s" state))
	(unless (numberp secs)
		(if (eq secs 'default)
			(setq secs (codeium-get-config 'codeium-delay nil state))))
	(run-with-timer secs nil #'codeium-defer-until-no-input
		state (codeium-state-alive-tracker state) func args))
;; (defun codeium-run-with-timer-with-tracker (state tracker secs func &rest args)
;; 	(when (eq tracker (codeium-state-alive-tracker state))
;; 		(apply #'codeium-run-with-timer state secs func args)))

(defun codeium-time-from (start-time)
	(float-time (time-subtract (current-time) start-time)))



;;;###autoload
(defun codeium-install (&optional state noconfirm)
	(interactive)
	(setq state (or state codeium-state))
	(when (codeium-state-alive-tracker state)
		(unless (yes-or-no-p "codeium is already running! are you sure to codeium-install? ") (user-error "aborted")))
	(setf (codeium-state-alive-tracker state)
		(gensym (codeium-state-name codeium-state)))
	(let*
		(
			(filename (codeium-get-config 'codeium-command-executable nil state))
			(url (codeium-get-config 'codeium-download-url nil state)))
		(when (file-exists-p filename)
			(unless (yes-or-no-p (format "%s already exists; overwrite? " filename)) (user-error "aborted")))
		(unless
			(or noconfirm
				(yes-or-no-p
					(format "you are about to download %s to %s. Proceed? " url filename)))
			(user-error "aborted"))
		(let ((log-callback (codeium-log-request state url)))
			(url-retrieve url
				(lambda (status)
					(when log-callback
						(funcall log-callback
							(let ((inhibit-read-only t) (print-escape-newlines t))
								(format " status: %s"
									(prin1-to-string
										(or
											(if url-http-response-status
												url-http-response-status status)
											"no status available"))))))
					(if (and url-http-response-status (<= 200 url-http-response-status) (<= url-http-response-status 299))
						(let ((url-buf (current-buffer)))
							(codeium-run-with-timer state 'default
								(lambda ()
									(codeium-install-process-url-res state url url-buf filename))))
						(message "codeium cannot fetch local language server: %s %s"
							status url-http-response-status)))
				nil 'silent 'inhibit-cookies))))

(defun codeium-install-process-url-res (state url url-buf filename)
	(make-directory (file-name-directory filename) t)
	(with-temp-file filename
		(set-buffer-multibyte nil)
		(url-insert-buffer-contents url-buf url)
		(unless (zlib-decompress-region (point-min) (point-max))
			(user-error "zlib is unable to decompress")))
	(chmod filename #o744)
	(kill-buffer url-buf)
	(message "successfully installed codeium local language server")
	(codeium-background-process-start state))


(defun codeium-request-callback (status state tracker callback log-callback)
	(let ((buf (current-buffer)))
		(when (eq tracker (codeium-state-alive-tracker state))
			(codeium-run-with-timer state 'default
				(lambda ()
					(when (buffer-live-p buf)
						(with-current-buffer buf
							(codeium-request-callback-process-res
								status callback log-callback))))))))
;; should be local to the url retrieve buffer
(defvar-local codeium-kill-url-retrieve-buffer t)
(defun codeium-request-callback-process-res (status callback log-callback)
	(when log-callback
		(let*
			((print-escape-newlines t)
				(status-str
					(format " status: %s"
						(prin1-to-string
							(or
								(if url-http-response-status
									url-http-response-status status)
								"no status available")))))
			(funcall log-callback status-str
				(if (and url-http-response-status (= url-http-response-status 200))
					"" status-str))))
	(funcall callback
		(let ((parsed 'error))
			(when url-http-end-of-headers
				(goto-char url-http-end-of-headers)
				(ignore-error json-parse-error
					(setq parsed (json-parse-buffer :object-type 'alist))))
			(when codeium-kill-url-retrieve-buffer
				(kill-buffer))
			(when (and parsed (not (eq parsed 'error)) log-callback)
				(funcall log-callback
					(let* ((print-escape-newlines t))
						(format " %s"
							(prin1-to-string
								(if (listp parsed)
									(or
										(alist-get 'state parsed)
										(alist-get 'message parsed)
										parsed)
									parsed))))
					(when-let
						((message-str
							 (and (listp parsed)
								 (or
									 (alist-get 'message (alist-get 'state parsed))
									 (alist-get 'message parsed)))))
						(when (stringp message-str)
							(concat " " message-str)))))
			parsed)))

(defun codeium-log-request (state str &optional mode-line-str mode-line-ttl)
	"print str on its own line in *codeium-log*, returns a callback function
that can add to that line."
	(let ((modeline-callback (when mode-line-str (codeium-log-mode-line state mode-line-str mode-line-ttl))))
		(when-let ((buf (codeium-get-config 'codeium-log-buffer nil state)))
			(with-current-buffer buf
				(let ((inhibit-read-only t)
						 time-beg-marker time-end-marker insert-marker
						 (start-time (current-time)))
					(save-excursion
						(goto-char (point-max))
						(beginning-of-line)
						(insert-before-markers "\n")
						(goto-char (1- (point)))
						(insert str)
						(insert " ")
						(setq time-beg-marker (point-marker))
						(insert codeium-log-waiting-text)
						(setq time-end-marker (point-marker))
						(set-marker-insertion-type time-end-marker t)
						(setq insert-marker (point-marker))
						(set-marker-insertion-type insert-marker t)
						(lambda (newstr &optional newstr-modeline modeline-append)
							(when (and newstr-modeline modeline-callback)
								(funcall modeline-callback newstr-modeline modeline-append))
							(when (buffer-live-p buf)
								(with-current-buffer buf
									(let ((inhibit-read-only t))
										(cl--set-buffer-substring time-beg-marker time-end-marker
											(format "%.2f secs" (codeium-time-from start-time)))
										(set-marker-insertion-type time-end-marker nil)
										(cl--set-buffer-substring insert-marker insert-marker
											newstr)
										(set-marker-insertion-type time-end-marker t)))))))))))

(defvar-local codeium-mode-line nil)
;; requirement for modeline
(put 'codeium-mode-line 'risky-local-variable t)


;; run user code in timers, for efficiency and infinite loop guard
(defvar-local codeium-modeline-refresh-scheduled nil)
(defun codeium-schedule-refresh-modeline-currentbuffer ()
	(unless codeium-modeline-refresh-scheduled
		(run-with-timer 0.005 nil #'codeium-refresh-modeline (current-buffer))
		(setq codeium-modeline-refresh-scheduled t)))
(defun codeium-refresh-modeline (buffer)
	(if (input-pending-p)
		(run-with-idle-timer 0.005 nil #'codeium-refresh-modeline buffer)
		(when (buffer-live-p buffer)
			(with-current-buffer buffer
				(unwind-protect
					(force-mode-line-update)
					(setq codeium-modeline-refresh-scheduled nil))))))

(defun codeium-remove-modeline-segment (segment buffer)
	(when (buffer-live-p buffer)
		(with-current-buffer buffer
			(setq codeium-mode-line (delq segment codeium-mode-line))
			(codeium-schedule-refresh-modeline-currentbuffer))))

(defun codeium-log-mode-line (_state str ttl)
	(let*
		(
			(segment `("[" nil (-30 ,str) "]"))
			(buffer (current-buffer))
			(start-time (current-time))
			(timer (run-with-timer ttl nil #'codeium-remove-modeline-segment segment buffer)))
		(push segment codeium-mode-line)
		(codeium-schedule-refresh-modeline-currentbuffer)
		(lambda (newstr &optional append)
			(cancel-timer timer)
			(when (buffer-live-p buffer)
				(with-current-buffer buffer
					(unless (memq segment codeium-mode-line) (push segment codeium-mode-line))
					(setq timer (run-with-timer ttl nil #'codeium-remove-modeline-segment segment buffer))
					(setf (nth 1 segment)
						(format "%.2fs" (codeium-time-from start-time)))
					(setf (nth 1 (nth 2 segment))
						(if append
							(concat (nth 1 (nth 2 segment)) newstr)
							newstr))
					(codeium-schedule-refresh-modeline-currentbuffer))))))

(defun codeium-request-with-body (api state body tracker callback)
	(when (eq tracker (codeium-state-alive-tracker state))
		(if (codeium-get-config 'codeium-port nil state)
			(let*
				(
					(url (codeium-get-config 'codeium-url api state))
					(url-request-method "POST")
					(url-request-extra-headers `(("Content-Type" . "application/json")))
					(url-request-data (encode-coding-string (json-serialize body) 'utf-8))
					(log-callback
						(codeium-log-request state (url-recreate-url url)
							(when (codeium-get-config 'codeium-mode-line-enable api state)
								(symbol-name api))
							(codeium-get-config 'codeium-mode-line-keep-time api state))))
				(when-let
					(
						(url-buf
							(url-retrieve url #'codeium-request-callback
								(list state (codeium-state-alive-tracker state) callback log-callback)
								'silent 'inhibit-cookies))
						(url-proc (get-buffer-process url-buf)))
					(set-process-query-on-exit-flag url-proc nil)))
			(codeium-on-port-ready state (lambda () (codeium-request-with-body api state body tracker callback))))))

(defun codeium-request (api state vals-alist callback)
	"make an async request to api, calls callback when done.
callback is called with a single argument, the return of
(json-parse-buffer :object-type \\='alist)

returns the body as returned by codeium-make-body-for-api
If `codeium-api-enabled' returns nil, does nothing.

"
	(unless (codeium-state-alive-tracker state)
		(error "codeium-state is not alive! %s" state))
	(when (codeium-get-config 'codeium-api-enabled api state)
		(let ((body (codeium-make-body-for-api api state vals-alist)))
			(codeium-request-with-body api state body (codeium-state-alive-tracker state) callback)
			body)))


(defun codeium-background-process-schedule (state)
	(codeium-run-with-timer state 'default #'codeium-background-process-start state))

(defun codeium-create-process (state)
	(let (buf (executable (car (codeium-get-config 'codeium-command nil state))))
		(unless (executable-find executable)
			(if (and (file-name-absolute-p executable) (not (file-exists-p executable)))
				(user-error "%s does not exist. use M-x codeium-install to install one"
					executable)
				(user-error "%s is not a valid executable. use M-x codeium-install to install one"
					executable)))
		(setq buf (codeium-get-config 'codeium-log-buffer nil state))
		(setf (codeium-state-proc state)
			(make-process
				:name "codeium"
				:connection-type 'pipe
				:buffer buf
				:coding 'no-conversion
				:command (codeium-get-config 'codeium-command nil state)
				:noquery t))))

(defun codeium-background-process-start (state)
	;; entrypoint
	;; since user calls start, cancel previous stuff
	(unless (codeium-state-alive-tracker state)
		(error "codeium-state is not alive! %s" state))
	(cond
		(;; we created the process but that is now dead for some reason
			(and (codeium-state-proc state)
				(not (process-live-p (codeium-state-proc state))))
			(codeium-reset state))
		((and
			 (not (codeium-get-config 'codeium-port nil state))
			 (not (codeium-get-config 'codeium-directory nil state))
			 (not (codeium-state-proc state)))
			(setf (codeium-state-port state) nil)
			(when-let ((dir (codeium-state-manager-directory state)))
				(delete-directory dir t)
				(setf (codeium-state-manager-directory state) nil))
			(codeium-create-process state)
			(codeium-background-process-schedule state))
		((not (codeium-get-config 'codeium-port nil state))
			(unless (codeium-get-config 'codeium-directory nil state)
				(error "no manager directory defined"))
			(let ((files
					  (directory-files (codeium-get-config 'codeium-directory nil state)
						  nil (rx bol (* num) eol))))
				(when files
					(setf (codeium-state-port state) (string-to-number (car files)))
					(mapc (lambda (func) (codeium-run-with-timer state 'default func))
						(codeium-state-port-ready-hook state)))
				(codeium-background-process-schedule state)))
		((and
			 (not (codeium-state-last-auth-token state))
			 (not (codeium-state-last-api-key state))
			 (not (codeium-get-config 'codeium/metadata/api_key 'GetCompletions state)))
			(let ((authurl (codeium-make-auth-url state)))
				(when (y-or-n-p (format "no codeium api-key found; visit %s to log in?" authurl))
					(browse-url authurl))
				(message "you can also use M-x codeium-kill-last-auth-url to copy the codeium login url"))
			(codeium-request 'GetAuthToken state nil
				(lambda (res)
					(if-let ((token (and (listp res) (alist-get 'authToken res))))
						(setf (codeium-state-last-auth-token state) token)
						(error "cannot get auth_token from res"))
					(codeium-background-process-schedule state))))

		((and
			 (not (codeium-state-last-api-key state))
			 (not (codeium-get-config 'codeium/metadata/api_key 'GetCompletions state)))
			(codeium-request 'RegisterUser state nil
				(lambda (res)
					(if-let ((key (and (listp res) (alist-get 'api_key res))))
						(progn
							(when (y-or-n-p "save codeium/metadata/api_key using customize?")
								(customize-save-variable 'codeium/metadata/api_key key))
							(setf (codeium-state-last-api-key state) key))
						(error "cannot get api_key from res"))
					(codeium-background-process-schedule state))))

		(t
			(codeium-request 'Heartbeat state nil
				(lambda (_res)
					(codeium-run-with-timer state 5 #'codeium-background-process-start state))))))

(defun codeium-reset (&optional state)
	(interactive)
	(setq state (or state codeium-state))
	(setf (codeium-state-alive-tracker state) nil)
	(when-let ((proc (codeium-state-proc state)))
		(delete-process proc)
		(setf (codeium-state-proc state) nil))
	(when-let ((dir (codeium-state-manager-directory state)))
		(delete-directory dir t)
		(setf (codeium-state-manager-directory state) nil))
	(setf (codeium-state-port state) nil)
	(setf (codeium-state-port-ready-hook state) nil)
	(setf (codeium-state-last-api-key state) nil)
	(setf (codeium-state-last-auth-token state) nil)
	(setf (codeium-state-results-table state) (make-hash-table :test 'eql :weakness nil))
	(setf (codeium-state-pending-table state) (make-hash-table :test 'eql :weakness nil)))


(defun codeium-on-port-ready (state callback)
	(if (codeium-state-port state)
		(funcall callback)
		(push callback (codeium-state-port-ready-hook state))))

(defun codeium-request-cancelrequest (state requestid)
	(codeium-request 'CancelRequest state
		`((codeium/request_id . ,requestid))
		#'ignore))

(defun codeium-request-synchronously (api state vals-alist)
	"sends request to codeium, return (reqbody . resbody) or nil
if user input is encountered, schedule a `CancelRequest' and return nil

this uses `sit-for', which means that timers can be ran while this function
waits, but these function called by timers must exit before this function
returns. Prefer using `codeium-request' directly instead.
"
	(when (not (input-pending-p))
		(let*
			(
				(tracker (codeium-state-alive-tracker state))
				(requestid (cl-incf (codeium-state-last-request-id state)))
				(_ (puthash requestid t (codeium-state-pending-table state)))
				(reqbody
					(codeium-request api state vals-alist
						(lambda (res)
							(when (gethash requestid (codeium-state-pending-table state))
								(remhash requestid (codeium-state-pending-table state))
								(puthash requestid res (codeium-state-results-table state))))))
				(rst 'noexist))
			(while (and (eq tracker (codeium-state-alive-tracker state)) (eq rst 'noexist) (not (input-pending-p)))
				(sit-for (codeium-get-config 'codeium-delay nil state))
				(setq rst (gethash requestid (codeium-state-results-table state) 'noexist)))
			(if (and (eq rst 'noexist) (eq tracker (codeium-state-alive-tracker state)))
				(when-let
					(
						(request-id-sent
							(codeium-nested-alist-get reqbody 'codeium/metadata/request_id))
						(buf (current-buffer)))
					(codeium-run-with-timer state 'default
						(lambda ()
							(when (buffer-live-p buf)
								(with-current-buffer buf
									(codeium-request-cancelrequest state request-id-sent))))))
				(remhash requestid (codeium-state-results-table state)))
			(if (or (eq rst 'error) (eq rst 'noexist)) nil (cons reqbody rst)))))

(defun codeium-utf8-byte-length (str)
	(length (encode-coding-string str 'utf-8)))
(defun codeium-make-utf8-offset-table (str offsets)
	(let*
		(
			(str-cur 0)
			(str-len (length str))
			(offset-cur 0)
			(offset-max (apply #'max 0 offsets))
			(table (make-hash-table :test 'eql :weakness nil :size (* 2 (length offsets)))))
		(mapc
			(lambda (offset)
				(puthash offset nil table))
			offsets)
		(while (and (< str-cur str-len) (<= offset-cur offset-max))
			(dotimes (_ (codeium-utf8-byte-length (substring-no-properties str str-cur (1+ str-cur))))
				(unless (eq (gethash offset-cur table 'noexist) 'noexist)
					(puthash offset-cur str-cur table))
				(cl-incf offset-cur))
			(cl-incf str-cur))
		(while (<= offset-cur offset-max)
			(puthash offset-cur str-len table)
			(cl-incf offset-cur))
		table))
(defmacro codeium-gv-map-table (gv table)
	`(setf ,gv (gethash (codeium-string-to-number-safe ,gv) ,table)))
(defmacro codeium-mapcar-mutate (func seq-gv)
	`(setf ,seq-gv (mapcar ,func ,seq-gv)))

(defun codeium-make-completion-string (completion-item document beg end)
	(let ((cur beg))
		(mapconcat
			(lambda (part)
				(when-let*
					(
						;; should be int since its been processed by codeium-parse-getcompletions-res-process-offsets
						(offset (alist-get 'offset part))
						(type (alist-get 'type part))
						(text (alist-get 'text part)))
					(when (or (string= type "COMPLETION_PART_TYPE_INLINE") (string= type "COMPLETION_PART_TYPE_BLOCK"))
						(prog1
							(concat
								(substring document cur (min offset (length document)))
								;; (substring document (min cur offset (length document)) (min offset (length document)))
								(when (string= type "COMPLETION_PART_TYPE_BLOCK") "\n")
								text)
							(setq cur offset)))))
			(append (alist-get 'completionParts completion-item) `(((offset . ,end))))
			"")))

(defun codeium-string-to-number-safe (str)
	(if (stringp str) (string-to-number str) str))
(defun codeium-parse-getcompletions-res-process-offsets (document cursor res)
	(let*
		(
			(items (alist-get 'completionItems res))
			(offsets-full-list
				(mapcar #'codeium-string-to-number-safe
					(remove nil
						(mapcan
							(lambda (item)
								(append
									(list
										(alist-get 'startOffset (alist-get 'range item))
										(alist-get 'endOffset (alist-get 'range item)))
									(mapcar
										(lambda (part) (alist-get 'offset part))
										(alist-get 'completionParts item))))
							items))))
			(offsets-table (codeium-make-utf8-offset-table document (push cursor offsets-full-list)))
			(_
				(codeium-mapcar-mutate
					(lambda (item)
						(codeium-gv-map-table (alist-get 'startOffset (alist-get 'range item)) offsets-table)
						(codeium-gv-map-table (alist-get 'endOffset (alist-get 'range item)) offsets-table)
						(codeium-mapcar-mutate
							(lambda (part)
								(codeium-gv-map-table (alist-get 'offset part) offsets-table)
								part)
							(alist-get 'completionParts item))
						item)
					items)))
		offsets-table))

;; WARNING: this mutates res
(defun codeium-parse-getcompletions-res (req res)
	"takes req and res"

	;; (setq res (cdr (codeium-request-synchronously 'GetCompletions codeium-state nil)))
	;; (mapcar 'car res)
	;; (state completionItems requestInfo)
	;; (alist-get 'state res)
	;; (alist-get 'requestInfo res)

	;; (setq items (alist-get 'completionItems res))
	;; (setq item (elt items 0))
	;; (mapcar 'car item)
	;; (completion range source completionParts)
	;; (alist-get 'completion item)
	;; (alist-get 'range item)
	;; (alist-get 'source item)
	;; (alist-get 'completionParts item)
	;; (alist-get 'endOffset (alist-get 'range item))

	;; (print (alist-get 'state res))
	;; (alist-get 'completionId (alist-get 'completion item))
	(when (alist-get 'completionItems res)
		(let*
			(
				(document (codeium-nested-alist-get req 'codeium/document/text))
				(cursor (codeium-nested-alist-get req 'codeium/document/cursor_offset))
				(items (alist-get 'completionItems res))
				(offset-hashtable (codeium-parse-getcompletions-res-process-offsets document cursor res))
				(cursor (gethash cursor offset-hashtable))
				offset-list
				(_
					(maphash (lambda (_ offset) (if offset (push offset offset-list))) offset-hashtable))
				(range-min (apply #'min cursor offset-list))
				(range-max (apply #'max cursor offset-list))
				(strings-list
					(mapcar
						(lambda (item) (codeium-make-completion-string item document range-min range-max))
						items))
				(completionids
					(mapcar
						(lambda (item)
							(alist-get 'completionId (alist-get 'completion item)))
						items)))
			;; (print (elt items 0))
			;; (print (alist-get 'completionParts (elt items 0)))
			;; (print strings-list)
			;; (print (alist-get 'completion (elt items 0)))
			;; ;; (print (alist-get 'range (elt items 0)))
			;; ;; (print (alist-get 'source (elt items 0)))
			;; (print (list (- range-min cursor) (- range-max cursor) (nth 0 strings-list)))
			;; (print (list range-min cursor range-max))
			(list (- range-min cursor) (- range-max cursor) strings-list completionids))))

;;;###autoload
(defun codeium-init (&optional state)
	(interactive)
	(setq state (or state codeium-state))
	(setf (codeium-state-alive-tracker state)
		(gensym (codeium-state-name codeium-state)))
	(condition-case err
		(codeium-background-process-start state)
		(error (codeium-reset state)
			(signal (car err) (cdr err)))))

;;;###autoload
(defun codeium-completion-at-point (&optional state)
	(setq state (or state codeium-state))
	(when
		(and (codeium-state-proc state)
			(not (process-live-p (codeium-state-proc state))))
		(codeium-reset state))
	(unless (codeium-state-alive-tracker state)
		(codeium-init state))
	;; (condition-case err
	(when-let*
		(
			(buffer-prev-str (buffer-string))
			(prev-point-offset (- (point) (point-min)))
			(tmp (codeium-request-synchronously 'GetCompletions state nil))
			(req (car tmp))
			(res (cdr tmp))
			(rst (and (not (input-pending-p)) (codeium-parse-getcompletions-res req res))))
		(cl-destructuring-bind (dmin dmax table completionids) rst
			(let*
				(
					(rmin (+ dmin (point)))
					(rmax (+ dmax (point)))
					(pmin (+ dmin prev-point-offset))
					(pmax (+ dmax prev-point-offset)))
				(when
					(and
						(<= (point-min) rmin)
						(<= rmax (point-max))
						(<= 0 pmin)
						(<= pmax (length buffer-prev-str))
						(string=
							(buffer-substring-no-properties rmin rmax)
							(substring-no-properties buffer-prev-str pmin pmax)))
					(list rmin rmax table :exit-function
						(lambda (string status)
							(when-let ((num (and (eq status 'finished) (cl-position string table :test 'string=))))
								(codeium-request 'AcceptCompletion state
									`((codeium/completion_id . ,(nth num completionids)))
									#'ignore))))))))
	;; (error
	;; 	(message "an error occurred in codeium-completion-at-point: %s" (error-message-string err))
	;; 	nil)
	;; )
	)

;; TODO: put these in separate file

(defun codeium-test ()
	(cl-letf*
		(
			(state (codeium-state-make :name "test"))
			;; ((codeium-config 'codeium/metadata/api_key state) (codeium-uuid-create))
			;; ((codeium-config 'codeium/document/text state) "def fibi(n):")
			;; ((codeium-config 'codeium/document/cursor_offset state) 12)
			((codeium-config 'codeium-api-enabled state) (lambda (api) (eq api 'GetCompletions))))
		(unwind-protect
			(codeium-completion-at-point state)
			(codeium-reset state))))

(defun codeium-test-cancel ()
	(let ((state (codeium-state-make :name "test")))
		(unwind-protect
			(cl-letf*
				(
					((codeium-config 'codeium-api-enabled state) (lambda (api) (memq api '(GetCompletions CancelRequest))))
					((codeium-config 'codeium/document/text state) "def fibi(n):")
					((codeium-config 'codeium/document/cursor_offset state) 12)
					(_ (codeium-init state))

					((codeium-config 'codeium/metadata/request_id state) 1)
					(_ (codeium-on-port-ready state
						   (lambda ()
							   (run-with-timer 0.001 nil
								   (lambda () (codeium-request 'CancelRequest state `((codeium/request_id . 1)) #'ignore)))))))
				(cdr (codeium-request-synchronously 'GetCompletions state nil)))
			(codeium-reset state))))

;; (makunbound 'state)
;; (setq state (codeium-test-cancel))
;; (codeium-reset state)
;; (codeium-state-background-process-cancel-fn state)


(defun codeium-test-multiround (round callback)
	(if (= round 0)
		(funcall callback)
		(codeium-test)
		(run-with-timer 0.005 nil 'codeium-test-multiround (1- round) callback)))

(defun codeium-stresstest ()
	"works by advising `url-retrieve'
so only run this when no other codeium or other code is using that"
	(let*
		(
			(n 50)
			(start-time (current-time))
			url-retrieve-buffer url-retrieve-status
			(url-retrieve-advise
				(lambda (func url callback &optional cbargs &rest args)
					(if url-retrieve-buffer
						(let
							(
								(callback-wrapped
									(lambda ()
										(with-current-buffer url-retrieve-buffer
											(setq codeium-kill-url-retrieve-buffer nil)
											(apply callback url-retrieve-status cbargs)))))
							(run-with-timer 0.005 nil callback-wrapped)
							url-retrieve-buffer)
						(let
							((callback-wrapped
								 (lambda (status)
									 (setq url-retrieve-buffer (current-buffer))
									 (setq url-retrieve-status status)
									 (setq codeium-kill-url-retrieve-buffer nil)
									 (apply callback status cbargs))))
							(apply func url callback-wrapped nil args))))))
		(advice-add 'url-retrieve :around url-retrieve-advise)
		(codeium-test-multiround n
			(lambda ()
				(message "average time: %s" (/ (float-time (time-subtract (current-time) start-time)) n))
				(advice-remove 'url-retrieve url-retrieve-advise)))))

;; (setq codeium-mode-line-enable t)
;; (run-with-timer 0.1 nil (lambda () (message "%s" (codeium-test))))
;; (dotimes (_ 10) (run-with-timer 0.1 nil 'codeium-stresstest))



(provide 'codeium)
;;; codeium.el ends here
