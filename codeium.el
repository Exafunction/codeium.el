;;; codeium.el --- codeium client for emacs         -*- lexical-binding: t; -*-

;; licence here

;;; Commentary:

;; currently you can install codeium binaries from https://github.com/Exafunction/codeium/releases/

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

(setf version "1.1.38")

(require 'url-parse) ; for url-parse-make-urlobj
;; removing this gives comp warnings, not sure why since its defined in url-http?
(eval-when-compile
	(defvar url-http-end-of-headers)
	(defvar url-http-response-status)
	(defvar help-xref-stack-item))

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
(defmacro codeium-def (name &rest args)
	(declare (doc-string 3))
	(pcase-let*
		(
			(doc-str) (value) (arglilst) (body)
			(
				(or
					`(,value)
					`(,value ,(and (pred stringp) doc-str))
					`(,arglist ,(and (pred stringp) doc-str) . ,body)
					`(,arglist . ,body)
					)
				args)
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


(codeium-def codeium-directory () nil)

(codeium-def codeium-command (api state)
	`(,(expand-file-name "codeium/codeium_language_server" user-emacs-directory)
		 "--api_server_host" "server.codeium.com"
		 "--api_server_port" "443" 
		 "--manager_dir" ,(codeium-state-manager-directory state)))

(defconst codeium-apis
	'(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect))

(codeium-def codeium-api-enabled (api) t)

(defvar codeium-fields-regexps
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
(codeium-def codeium/metadata/extension_version "1.1.32")
(codeium-def codeium/metadata/ide_version (emacs-version))
;; (codeium-def codeium/metadata/request_id (api)
;; 	(when (eq api 'GetCompletions)
;; 		(random most-positive-fixnum)))

(defvar codeium-global-requestid-counter 0)
(codeium-def codeium/metadata/request_id (api state)
	(when (eq api 'GetCompletions)
		(cl-incf codeium-global-requestid-counter)))

;; for CancelRequest
(codeium-def codeium/request_id (api state val) val)

;; alternative getting key from file?
;; TODO
;; (setq codeium/metadata/api_key 'codeium-default/metadata/api_key)
(defun codeium-get-saved-api-key ())
(codeium-def codeium/metadata/api_key (api state)
	(if-let ((api-key (or (codeium-state-last-api-key state) (codeium-get-saved-api-key))))
		(setq codeium/metadata/api_key api-key)
		(setq codeium/metadata/api_key
			(lambda (api state)
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
		 (text-mode . 30)
		 (python-mode . 33)
		 (lisp-data-mode . 60)
		 ))
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

(codeium-def codeium/firebase_id_token (api state) (codeium-state-last-auth-token state))


(cl-defstruct
	(codeium-state
		(:constructor codeium-state-make)
		(:copier nil))
	(name "")
	config
	proc
	manager-directory
	port
	port-ready-callback-list
	background-process-cancel-fn

	last-auth-token
	last-api-key

	(last-request-id 0)
	;; these has distinct elements
	(results-table (make-hash-table :test 'eql :weakness nil)) ; results that are ready
	(pending-table (make-hash-table :test 'eql :weakness nil)) ; requestid that we are waiting for
	)


(defvar codeium-state (codeium-state-make :name "default"))

(defun codeium-config (field &optional state)
	(setq state (or state codeium-state))
	(if (eq (alist-get field (codeium-state-config state) 'noexist) 'noexist)
		(symbol-value field)
		(alist-get field (codeium-state-config state))))
(defun codeium--set-config (val field &optional state)
	(setq state (or state codeium-state))
	(setf (alist-get field (codeium-state-config state)) val))
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
		(mapc
			(lambda (api)
				(when (codeium-get-config 'codeium-api-enabled api state)
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
							(string-fill " " 5)
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

(codeium-def codeium-log-buffer (state)
	(let ((buf (get-buffer "*codeium-log*")))
		(if buf buf
			(setq buf (generate-new-buffer "*codeium-log*"))
			(with-current-buffer buf
				(special-mode))
			buf)))

(defun codeium-ensure-process (state)
	(if (codeium-get-config 'codeium-directory nil state)
		(setf (codeium-state-manager-directory state) (codeium-get-config 'codeium-directory nil state))
		(let ((proc (codeium-state-proc state)) buf
				 (executable (car (codeium-get-config 'codeium-command nil state))))
			(unless (executable-find executable)
				(if (and (file-name-absolute-p executable) (not (file-exists-p executable)))
					(error "%s does not exist. use M-x codeium-install to install one"
						executable)
					(error "%s is not a valid executable. use M-x codeium-install to install one"
						executable)))
			(unless (and proc (process-live-p proc))
				(setq buf (codeium-get-config 'codeium-log-buffer nil state))
				(unless (codeium-state-manager-directory state)
					(setf (codeium-state-manager-directory state) (make-temp-file "codeium_" t)))
				(setq proc
					(make-process
						:name "codeium"
						:connection-type 'pipe
						:buffer buf
						:coding 'no-conversion
						:command (codeium-get-config 'codeium-command nil state)
						:noquery t))
				(setf (codeium-state-proc state) proc))
			proc)))

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
				(url-recreate-url
					(url-parse-make-urlobj "https" nil nil "www.codeium.com" nil (concat "/profile?" query-params) nil nil t))))
		(setq codeium-last-auth-url url)))
(defun codeium-kill-last-auth-url ()
	(interactive)
	(when codeium-last-auth-url
		(message "%s sent to kill-ring" codeium-last-auth-url)
		(kill-new codeium-last-auth-url)))

(defun codeium-defer-until-no-input (func &optional args)
	(if (input-pending-p)
		(run-with-idle-timer 0.005 nil #'codeium-defer-until-no-input func args)
		(apply func args)))
(defun codeium-run-with-timer (secs func &rest args)
	(run-with-timer secs nil #'codeium-defer-until-no-input func args))

(defun codeium-time-from (start-time)
	(float-time (time-subtract (current-time) start-time)))

(defun codeium-async-process (command-list callback &optional buffer)
	(make-process
		:name "codeium-async-process"
		:buffer buffer 
		:command command-list
		:noquery t
		:sentinel
		(lambda (proc str)
			(when (string= str "finished\n")
				(codeium-defer-until-no-input callback)))))

(defun get-operating-system-architecture ()
  (let ((os (string-trim (shell-command-to-string "uname -s")))
        (arch (string-trim (shell-command-to-string "uname -m"))))
    (list os arch)))

(defun get-language-server-string ()
  (let* ((os-arch (get-operating-system-architecture))
        (is-arm (or (string-match-p "arm" (cadr os-arch))
                    (string-match-p "aarch64" (cadr os-arch)))))
    (cond
     ((and (string= (car os-arch) "Linux") is-arm)
      "language_server_linux_arm")
     ((and (string= (car os-arch) "Linux") (string= (cadr os-arch) "x86_64"))
      "language_server_linux_x64")
     ((and (string= (car os-arch) "Darwin") (string= (cadr os-arch) "x86_64"))
      "language_server_macos_x64")
     ((and (string= (car os-arch) "Darwin") is-arm)
      "language_server_macos_arm")
     ((string= (car os-arch) "Windows")
      "language_server_windows_x64.exe")
		 (t (car os-arch)))))

;;;###autoload
(defun codeium-install (&optional callback state noconfirm)
	(interactive)
	(setq state (or state codeium-state))
	(let*
		(
			(filename (car (codeium-get-config 'codeium-command nil state)))
			(language-server-string (get-language-server-string))
			(url (concat "https://github.com/Exafunction/codeium/releases/download/language-server-v" version "/" language-server-string ".gz")))
		(when (file-exists-p filename)
			(unless (yes-or-no-p (format "%s alreay exist; overwrite? " filename)) (error "aborted")))
		(unless
			(yes-or-no-p
				(format "you are about to download %s to %s. Proceed? " url filename))
			(error "aborted"))
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
					(let ((url-buf (current-buffer)))
						(codeium-run-with-timer 0.005
							(lambda ()
								(codeium-install-process-url-res url url-buf filename state callback)))))
				nil 'silent 'inhibit-cookies))))

(defun codeium-install-process-url-res (url url-buf filename state callback)
	(with-temp-file filename
		(set-buffer-multibyte nil)
		(url-insert-buffer-contents url-buf url)
		(unless (zlib-decompress-region (point-min) (point-max))
			(error "zlib fails")))
	(chmod filename #o744)
	(kill-buffer url-buf)
	(messgae "successfully installed codeium binary")
	(when callback
		(funcall callback)))


;; should be local to the url retrieve buffer
(defvar-local codeium-kill-url-retrieve-buffer t)
(defun codeium-request-callback (status callback log-callback)
	(if (input-pending-p)
		(let ((url-buffer (current-buffer)))
			;; note: we dont need to save say url-http-end-of-headers
			;; bc its local to url-buffer
			(run-with-idle-timer 0.005 nil
				(lambda ()
					(cl-letf (((current-buffer) url-buffer))
						(codeium-request-callback status callback request-time log-marker)))))
		(when log-callback
			(funcall log-callback
				(let ((inhibit-read-only t) (print-escape-newlines t))
					(format " status: %s"
						(prin1-to-string
							(or
								(if url-http-response-status
									url-http-response-status status)
								"no status available"))))))
		(funcall callback
			(if url-http-end-of-headers
				(let (parsed)
					(goto-char url-http-end-of-headers)
					(setq parsed (json-parse-buffer :object-type 'alist))
					;; (switch-to-buffer (current-buffer))
					(when codeium-kill-url-retrieve-buffer
						(kill-buffer))
					(when log-callback
						(funcall log-callback
							(let ((inhibit-read-only t) (print-escape-newlines t))
								(format " %s"
									(prin1-to-string
										(if (listp parsed)
											(or
												(alist-get 'state parsed)
												(alist-get 'message parsed)
												parsed)
											parsed))))))
					parsed)
				'error))))

(defun codeium-log-request (state str)
	"print str on its own line in *codeium-log*, returns a callback function
that can add to that line."
	(when-let ((buf (codeium-get-config 'codeium-log-buffer nil state)))
		(with-current-buffer buf
			(let ((inhibit-read-only t)
					 time-beg-marker time-end-marker insert-marker
					 (start-time (current-time)))
				(save-excursion
					(goto-char (point-max))
					(beginning-of-line)
					(insert-before-markers "\n")
					(cl-decf (point))
					(insert str)
					(insert " ")
					(setq time-beg-marker (point-marker))
					(insert codeium-log-waiting-text)
					(setq time-end-marker (point-marker))
					(set-marker-insertion-type time-end-marker t)
					(setq insert-marker (point-marker))
					(set-marker-insertion-type insert-marker t)
					(lambda (newstr)
						(when (buffer-live-p buf)
							(with-current-buffer buf
								(let ((inhibit-read-only t))
									(setf (buffer-substring time-beg-marker time-end-marker)
										(format "%.2f secs" (codeium-time-from start-time)))
									(set-marker-insertion-type time-end-marker nil)
									(setf (buffer-substring insert-marker insert-marker)
										newstr)
									(set-marker-insertion-type time-end-marker t))))))))))

(defun codeium-request-with-body (state api body callback)
	(if (codeium-state-port state)
		(let*
			(
				(url (codeium-get-config 'codeium-url api state))
				(url-request-method "POST")
				(url-request-extra-headers `(("Content-Type" . "application/json")))
				(url-request-data (encode-coding-string (json-serialize body) 'utf-8))
				(log-callback (codeium-log-request state (url-recreate-url url))))
			(when-let
				(
					(url-buf (url-retrieve url 'codeium-request-callback (list callback log-callback) 'silent 'inhibit-cookies))
					(url-proc (get-buffer-process url-buf)))
				(set-process-query-on-exit-flag url-proc nil)))
		(codeium-on-port-ready state (lambda () (codeium-request-with-body state api body callback)))))

(defun codeium-request (state api vals-alist callback)
	"returns the body as returned by codeium-make-body-for-api, or nil if not codeium-api-enabled"
	(when (codeium-get-config 'codeium-api-enabled api state)
		(let ((body (codeium-make-body-for-api api state vals-alist)))
			(codeium-request-with-body state api body callback)
			body)))

(defun codeium-background-process-next (state)
	;; cancel-fn stores the function to cancel this function
	;; so we safely removes it
	(setf (codeium-state-background-process-cancel-fn state) nil)
	(codeium-background-process-start state))
(defun codeium-background-process-request (state api vals-alist callback)
	;; when the request is completed, we check if there has been any call to cancel earlier
	(let ((tracker (make-symbol "cancel-distinct-tracker")))
		(fset tracker #'ignore)
		(setf (codeium-state-background-process-cancel-fn state) tracker)
		(codeium-request state api vals-alist
			(lambda (res)
				(when (eq (codeium-state-background-process-cancel-fn state) tracker)
					(funcall callback res))))))
(defun codeium-background-process-start (state)
	;; entrypoint
	;; since user calls start, cancel previous stuff
	(when (codeium-state-background-process-cancel-fn state)
		(funcall (codeium-state-background-process-cancel-fn state))
		(setf (codeium-state-background-process-cancel-fn state) nil))
	(cond
		((input-pending-p)
			(let ((timer (run-with-idle-timer 0.005 nil #'codeium-background-process-next state)))
				(setf (codeium-state-background-process-cancel-fn state) (lambda () (cancel-timer timer)))))
		((and (not (codeium-state-manager-directory state))
			 (or (not (codeium-state-proc state))
				 (not (process-live-p (codeium-state-proc state)))))
			(setf (codeium-state-port state) nil)
			(codeium-ensure-process state)
			(codeium-background-process-start state))
		((not (codeium-state-port state))
			(unless (codeium-state-manager-directory state) 
				(error "no manager directory defined"))
			(let ((files
					  (directory-files (codeium-state-manager-directory state)
						  nil (rx bol (* num) eol))))
				(if files
					(progn
						(setf (codeium-state-port state) (string-to-number (car files)))
						(while (codeium-state-port-ready-callback-list state)
							(let ((callbacks (codeium-state-port-ready-callback-list state)))
								(setf (codeium-state-port-ready-callback-list state) nil)
								(mapc #'funcall callbacks)))
						(codeium-background-process-next state))
					(let ((timer (run-with-timer 0.005 nil #'codeium-background-process-next state)))
						(setf (codeium-state-background-process-cancel-fn state) (lambda () (cancel-timer timer)))))))

		((and
			 (not (codeium-state-last-auth-token state))
			 (not (codeium-state-last-api-key state))
			 (not (codeium-get-config 'codeium/metadata/api_key 'GetCompletions state)))
			(let ((authurl (codeium-make-auth-url state)))
				(when (y-or-n-p (format "no codeium api-key found; visit %s to log in?" authurl))
					(browse-url authurl))
				(message "you can also use M-x codeium-kill-last-auth-url to copy the codeium login url"))
			(codeium-background-process-request state 'GetAuthToken nil
				(lambda (res)
					(if-let ((_ (listp res)) (token (alist-get 'authToken res)))
						(setf (codeium-state-last-auth-token state) token)
						(error "cannot get auth_token from res"))
					(codeium-background-process-next state))))

		((and
			 (not (codeium-state-last-api-key state))
			 (not (codeium-get-config 'codeium/metadata/api_key 'GetCompletions state)))
			(codeium-background-process-request state 'RegisterUser nil
				(lambda (res)
					(if-let ((_ (listp res)) (key (alist-get 'api_key res)))
						(progn
							(when (y-or-n-p "save codeium/metadata/api_key using customize?")
								(customize-save-variable 'codeium/metadata/api_key key))
							(setf (codeium-state-last-api-key state) key))
						(error "cannot get api_key from res"))
					(codeium-background-process-next state))))

		(t
			(codeium-background-process-request state 'Heartbeat nil
				(lambda (res)
					(let ((timer (run-with-timer 5 nil #'codeium-background-process-next state)))
						(setf (codeium-state-background-process-cancel-fn state) (lambda () (cancel-timer timer)))))))))

(defun codeium-reset (&optional state)
	(interactive)
	(setq state (or state codeium-state))
	(when (codeium-state-background-process-cancel-fn state)
		(funcall (codeium-state-background-process-cancel-fn state))
		(setf (codeium-state-background-process-cancel-fn state) nil))
	(when-let ((proc (codeium-state-proc state)))
		(delete-process proc)
		(setf (codeium-state-proc state) nil))
	;; TODO: what if the user changes this? i should actually track whether i made the directory
	(if (not (codeium-get-config 'codeium-directory nil state))
		(when-let ((dir (codeium-state-manager-directory state)))
			(delete-directory dir t)
			(setf (codeium-state-manager-directory state) nil)))
	(setf (codeium-state-port state) nil)
	(setf (codeium-state-port-ready-callback-list state) nil)
	(setf (codeium-state-last-api-key state) nil)
	(setf (codeium-state-last-auth-token state) nil)
	(setf (codeium-state-results-table state) (make-hash-table :test 'eql :weakness nil))
	(setf (codeium-state-pending-table state) (make-hash-table :test 'eql :weakness nil)))


(defun codeium-on-port-ready (state callback)
	(unless (codeium-state-background-process-cancel-fn state)
		(codeium-background-process-start state))
	(if (codeium-state-port state)
		(funcall callback)
		(push callback (codeium-state-port-ready-callback-list state))))

(defun codeium-request-cancelrequest (state requestid)
	(codeium-request state 'CancelRequest
		`((codeium/request_id . ,requestid))
		#'ignore))

(defun codeium-request-getcompletions-while-noinput (state)
	"sends 'GetCompletions request to codeium, return (reqbody . resbody) or nil
if user input is encountered, send a 'CancelRequest before returning nil

this uses `sit-for', which means that timers can be ran while this function
waits, but these function called by timers must exit before this function
returns. Due to this you should prefer not calling this;
I'll eventually make an async version, but for now
use `codeium-request' directly instead
" 
	(unless (input-pending-p)
		(let*
			(
				(requestid (cl-incf (codeium-state-last-request-id state)))
				(_ (puthash requestid t (codeium-state-pending-table state)))
				(reqbody
					(codeium-request state 'GetCompletions nil
						(lambda (res)
							(when (gethash requestid (codeium-state-pending-table state))
								(remhash requestid (codeium-state-pending-table state))
								(puthash requestid res (codeium-state-results-table state))))))
				(rst 'noexist))
			(while (and (eq rst 'noexist) (not (input-pending-p)))
				(sit-for 0.005)
				(setq rst (gethash requestid (codeium-state-results-table state) 'noexist)))
			(if (eq rst 'noexist)
				(codeium-request-cancelrequest state
					(codeium-nested-alist-get reqbody 'codeium/metadata/request_id))
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
			(dotimes (i (codeium-utf8-byte-length (substring-no-properties str str-cur (1+ str-cur))))
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
						(text (alist-get 'text part))
						(_ (or (string= type "COMPLETION_PART_TYPE_INLINE") (string= type "COMPLETION_PART_TYPE_BLOCK")))
						)
					(prog1
						(concat
							(substring document cur (min offset (length document)))
							;; (substring document (min cur offset (length document)) (min offset (length document)))
							(when (string= type "COMPLETION_PART_TYPE_BLOCK") "\n")
							text)
						(setq cur offset))))
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
						items)))
			;; (print (elt items 0))
			;; (print (alist-get 'completionParts (elt items 0)))
			;; (print strings-list)
			;; (print (alist-get 'completion (elt items 0)))
			;; ;; (print (alist-get 'range (elt items 0)))
			;; ;; (print (alist-get 'source (elt items 0)))
			;; (print (list (- range-min cursor) (- range-max cursor) (nth 0 strings-list)))
			;; (print (list range-min cursor range-max))
			(list (- range-min cursor) (- range-max cursor) strings-list))))

;;;###autoload
(defun codeium-init (&optional state)
	(interactive)
	(setq state (or state codeium-state))
	(codeium-background-process-start state))

;;;###autoload
(defun codeium-completion-at-point (&optional state)
	(setq state (or state codeium-state))
	;; (condition-case err
	(when-let*
		(
			(buffer-prev-str (buffer-string))
			(prev-point-offset (- (point) (point-min)))
			(tmp (codeium-request-getcompletions-while-noinput state))
			(req (car tmp))
			(res (cdr tmp))
			(rst (codeium-parse-getcompletions-res req res)))
		(cl-destructuring-bind (dmin dmax table) rst
			(when-let*
				(
					(rmin (+ dmin (point)))
					(rmax (+ dmax (point)))
					(pmin (+ dmin prev-point-offset))
					(pmax (+ dmax prev-point-offset))
					(_ (<= (point-min) rmin))
					(_ (<= rmax (point-max)))
					(_ (<= 0 pmin))
					(_ (<= pmax (length buffer-prev-str)))
					(_ (string=
						   (buffer-substring-no-properties rmin rmax)
						   (substring-no-properties buffer-prev-str pmin pmax))))
				(list rmin rmax table))))
	;; (error
	;; 	(message "an error occured in codeium-completion-at-point: %s" (error-message-string err))
	;; 	nil)
	;; )
	)

;; TODO: put these in seperate file

(defun codeium-test ()
	(cl-letf*
		(
			(state (codeium-state-make :name "test"))
			;; ((codeium-config 'codeium/metadata/api_key state) (codeium-uuid-create))
			;; ((codeium-config 'codeium/document/text state) "def fibi(n):")
			;; ((codeium-config 'codeium/document/cursor_offset state) 12)
			((codeium-config 'codeium-api-enabled state) (lambda (api) (eq api 'GetCompletions))))
		(prog1
			(codeium-completion-at-point state)
			(codeium-reset state))))

(defun codeium-test-multiround (round callback)
	(if (= round 0)
		(funcall callback)
		(codeium-test)
		(codeium-run-with-timer 0.005 'codeium-test-multiround (1- round) callback)))

(defun codeium-stresstest ()
	"works by advising `url-retrieve' so only run this when no other codeium or other code is using that"
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
										(cl-letf
											(
												(codeium-kill-url-retrieve-buffer nil)
												((current-buffer) url-retrieve-buffer))
											(apply callback url-retrieve-status cbargs)))))
							(codeium-run-with-timer 0.005 callback-wrapped)
							url-retrieve-buffer)
						(let
							((callback-wrapped
								 (lambda (status)
									 (setq url-retrieve-buffer (current-buffer))
									 (setq url-retrieve-status status)
									 (let ((codeium-kill-url-retrieve-buffer nil))
										 (apply callback status cbargs)))))
							(apply func url callback-wrapped nil args))))))
		(advice-add 'url-retrieve :around url-retrieve-advise)
		(codeium-test-multiround n
			(lambda ()
				(message "average time: %s" (/ (float-time (time-subtract (current-time) start-time)) n))
				(advice-remove 'url-retrieve url-retrieve-advise)))))

;; (run-with-timer 0.1 nil (lambda () (message "%s" (codeium-test))))
;; (dotimes (_ 5) (run-with-timer 0.1 nil 'codeium-stresstest))

;; (defun 
(provide 'codeium)
;;; codeium.el ends here
