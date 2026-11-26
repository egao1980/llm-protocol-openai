(in-package #:llm-protocol-openai)

;;; 0.8.0 — errors and context windows (cl-stack-code H3f).
;;;
;;; The backend maps every failure onto llm-protocol 0.7.0's taxonomy, so a
;;; caller binds on a type and never parses an OpenAI-compatible server's
;;; error text itself. The rules:
;;;
;;; HTTP errors. %DECODE and the streaming path signal
;;;   (MAKE-LLM-HTTP-ERROR status :body body :message <the server's
;;;   error.message, else "HTTP <status>"> :retry-after <Retry-After>
;;;   :provider "openai" :url <base URL> :model <the request's model>),
;;;   with the same RETRY / USE-VALUE restarts as 0.6. Before that, a 400 or
;;;   413 whose message, error.code, error.type or body satisfies
;;;   OPENAI-CONTEXT-OVERFLOW-TEXT-P is signalled as LLM-CONTEXT-OVERFLOW
;;;   (MAKE-LLM-HTTP-ERROR already catches llm-protocol's generic phrases;
;;;   this adds the servers' own). Never the API key in any slot or message:
;;;   the exact key (8 characters or more) is also scrubbed from the server's
;;;   message and body.
;;;
;;; Retry-After. %HTTP-REQUEST returns a third value, the Retry-After header
;;;   string (or NIL). A custom :REQUEST-FN may return one too (a string or
;;;   seconds); two values mean none. MAKE-LLM-HTTP-ERROR parses it.
;;;
;;; Stream errors. An SSE `error` event (chat or Responses) is signalled
;;;   through MAKE-LLM-HTTP-ERROR as well: status = the event's integer
;;;   "status" or error "code" when there is one, else 500, so a mid-stream
;;;   server failure is retryable; the overflow and "overloaded" rules apply
;;;   to its message and type. (0.6 signalled retryable-p NIL with no status.)
;;;   Only an integer from 100 to 599 is a status (a server's own error
;;;   number is not). An event with no status whose text satisfies
;;;   OPENAI-CONTEXT-OVERFLOW-TEXT-P is LLM-CONTEXT-OVERFLOW (status 500): an
;;;   overflow never succeeds on retry.
;;;
;;; Transport. Inside %REQUEST (and while a streamed body is read) any
;;;   condition for which LLM-PROTOCOL:TRANSPORT-ERROR-KIND-FOR returns a kind
;;;   is translated (HANDLER-CASE) into LLM-TRANSPORT-ERROR with :CAUSE the
;;;   original, :KIND, :PROVIDER "openai", :URL the base URL, and the message
;;;   "<kind text> talking to <base URL>: <cause>" (kind text: "connection
;;;   failed", "connection reset", "TLS failed", "timed out", "canceled").
;;;   HTTP-STATUS-ERRORs never reach this (statuses are values here).
;;;   The translated condition is also every http-protocol type its cause was
;;;   (its class mixes the cause's most specific http-protocol transport type
;;;   into LLM-TRANSPORT-ERROR: a timeout is still an HTTP-TIMEOUT-ERROR, a
;;;   cut body still an HTTP-INCOMPLETE-RESPONSE), so handlers written for
;;;   http-protocol types still match. An LLM-ERROR is never translated (no
;;;   double wrapping), and neither is a condition the caller's :ON-PART
;;;   signals (not this backend's transport).
;;;
;;; Timeout: not here. #13 (the request timeout initarg, the idle timeout
;;;   for streams, no blind retry after a stream started) is fixed separately
;;;   and lands first; this contract does not re-specify it.
;;;
;;; Context windows. OPENAI-MODEL-CONTEXT-WINDOW backend model → an integer
;;;   or NIL: GET <base>/models once per backend (cached, also when it
;;;   fails), the entry whose "id" is MODEL, its first positive integer of
;;;   "max_context_length", "loaded_context_length", "context_length",
;;;   "max_model_len" (LM Studio, OpenRouter, vLLM). Any failure → NIL, never
;;;   a condition. LLM-PROTOCOL:CONTEXT-WINDOW on this backend: the catalog's
;;;   value first (0.6), else OPENAI-MODEL-CONTEXT-WINDOW. The catalog step
;;;   reads *LLM-CATALOG* and makes no request; LIST-MODELS stays a fresh
;;;   GET and refreshes the cached listing.

(defparameter +openai-context-overflow-markers+
  '("context_length_exceeded" "exceed_context_size_error"
    "exceeds the available context size" "greater than the context length"
    "context the overflows" "maximum context length")
  "Lower-case phrases OpenAI-compatible servers (OpenAI, llama.cpp, LM Studio,
vLLM) use for a prompt that does not fit.")

(defun openai-context-overflow-text-p (text)
  "T when TEXT is a string that contains one of +OPENAI-CONTEXT-OVERFLOW-MARKERS+,
ignoring case; NIL for anything else (NIL, numbers, :NULL)."
  (and (stringp text)
       (some (lambda (marker) (search marker text :test #'char-equal))
             +openai-context-overflow-markers+)
       t))

;;; --- HTTP errors -------------------------------------------------------------

(defun %scrub-api-key (backend text)
  "TEXT with each occurrence of BACKEND's exact API key replaced by
\"[redacted]\". A key shorter than 8 characters is left alone (it could match
ordinary words), and anything that is not a string is returned as given."
  (let ((key (and backend (openai-api-key backend))))
    (if (and (stringp text) (stringp key) (>= (length key) 8) (search key text))
        (with-output-to-string (out)
          (loop for start = 0 then (+ hit (length key))
                for hit = (search key text :start2 start)
                do (write-string text out :start start :end hit)
                while hit
                do (write-string "[redacted]" out)))
        text)))

(defun %openai-http-error (backend status &key body message code type retry-after model)
  "The condition object (made, not signalled) for an error answer (rules: this
file's header). STATUS is the HTTP status, or NIL for a stream error event that
carried none (then 500). A 400 or 413, or a status-less event, whose MESSAGE,
CODE, TYPE or BODY satisfies OPENAI-CONTEXT-OVERFLOW-TEXT-P is
LLM-CONTEXT-OVERFLOW; anything else is MAKE-LLM-HTTP-ERROR's class. MESSAGE that
is not a string becomes \"HTTP <status>\". The provider is \"openai\", the URL
is BACKEND's base URL, the model is MODEL. BACKEND's key is scrubbed from
MESSAGE and BODY."
  (let* ((overflow-status-p (or (null status) (member status '(400 413))))
         (status (or status 500))
         (message (%scrub-api-key backend (if (stringp message)
                                              message
                                              (format nil "HTTP ~a" status))))
         (body (%scrub-api-key backend body))
         (url (and backend (openai-base-url backend))))
    (if (and overflow-status-p
             (some #'openai-context-overflow-text-p (list message code type body)))
        (make-condition 'llm-context-overflow
                        :status status :body body :message message
                        :retry-after (parse-retry-after retry-after)
                        :provider "openai" :url url :model model)
        (make-llm-http-error status :body body :message message
                                    :retry-after retry-after
                                    :provider "openai" :url url :model model))))

(defun %signal-openai-http-error (condition)
  "Signal CONDITION (an LLM-HTTP-ERROR) with 0.6's restarts. RETRY re-runs the
operation through the outer RETRY (WITH-LLM-RESTARTS, CALL-WITH-RETRY-POLICY).
USE-VALUE returns VALUE from here, in place of the decoded body."
  ;; The literal ERROR form associates the restarts with CONDITION.
  (restart-case (error condition)
    (retry ()
      :report "Retry the HTTP request"
      (llm-protocol::%invoke-retry))
    (use-value (value)
      :report "Use a supplied decoded object"
      value)))

;;; --- transport errors --------------------------------------------------------

(defvar *%openai-transport-scope* nil
  "True while this backend talks to its server (%CALL-WITH-OPENAI-TRANSPORT-ERRORS);
NIL inside the caller's ON-PART (%EMIT-PART), whose failures stay the caller's.")

(define-condition openai-transport-incomplete
    (llm-transport-error http-protocol:http-incomplete-response) ()
  (:documentation "An LLM-TRANSPORT-ERROR whose cause is an
HTTP-INCOMPLETE-RESPONSE, and still one (so also an HTTP-PROTOCOL-ERROR and an
HTTP-CONNECTION-ERROR): handlers written before 0.8.0 match it."))

(define-condition openai-transport-canceled
    (llm-transport-error http-protocol:http-canceled) ()
  (:documentation "An LLM-TRANSPORT-ERROR whose cause is an HTTP-CANCELED, and
still one."))

(define-condition openai-transport-timeout
    (llm-transport-error http-protocol:http-timeout-error) ()
  (:documentation "An LLM-TRANSPORT-ERROR whose cause is an HTTP-TIMEOUT-ERROR
(the request timeout, or a stream's idle timeout), and still one."))

(define-condition openai-transport-tls
    (llm-transport-error http-protocol:http-tls-error) ()
  (:documentation "An LLM-TRANSPORT-ERROR whose cause is an HTTP-TLS-ERROR, and
still one."))

(define-condition openai-transport-connection
    (llm-transport-error http-protocol:http-connection-error) ()
  (:documentation "An LLM-TRANSPORT-ERROR whose cause is an HTTP-CONNECTION-ERROR
(refused, unreachable, reset or closed), and still one."))

(define-condition openai-transport-protocol
    (llm-transport-error http-protocol:http-protocol-error) ()
  (:documentation "An LLM-TRANSPORT-ERROR whose cause is an HTTP-PROTOCOL-ERROR
with a transport kind (\"incomplete HTTP response\" before
HTTP-INCOMPLETE-RESPONSE had a type), and still one."))

(define-condition openai-transport-http
    (llm-transport-error http-protocol:http-error) ()
  (:documentation "An LLM-TRANSPORT-ERROR whose cause is an HTTP-ERROR of no more
specific transport type, and still one."))

(defgeneric %transport-error-class (cause)
  (:documentation "The class of the LLM-TRANSPORT-ERROR that wraps CAUSE: the one
that mixes in CAUSE's most specific http-protocol transport type (CLOS
dispatch), so the wrapper is still every http-protocol type CAUSE was. A cause
that is no HTTP-ERROR (a kind from its text alone) gets a plain
LLM-TRANSPORT-ERROR.")
  (:method ((cause condition)) 'llm-transport-error)
  (:method ((cause http-protocol:http-error)) 'openai-transport-http)
  (:method ((cause http-protocol:http-protocol-error)) 'openai-transport-protocol)
  (:method ((cause http-protocol:http-connection-error)) 'openai-transport-connection)
  (:method ((cause http-protocol:http-incomplete-response)) 'openai-transport-incomplete)
  (:method ((cause http-protocol:http-timeout-error)) 'openai-transport-timeout)
  (:method ((cause http-protocol:http-tls-error)) 'openai-transport-tls)
  (:method ((cause http-protocol:http-canceled)) 'openai-transport-canceled))

(defparameter +%transport-kind-texts+
  '((:connect . "connection failed") (:reset . "connection reset")
    (:tls . "TLS failed") (:timeout . "timed out") (:canceled . "canceled"))
  "Transport kind → the opening words of the message (this file's header).")

(defun %openai-transport-error (backend cause kind)
  "The LLM-TRANSPORT-ERROR (made, not signalled) for CAUSE, a transport
condition of KIND (rules: this file's header). Its class is
%TRANSPORT-ERROR-CLASS's; :CAUSE CAUSE, :KIND KIND, provider \"openai\",
BACKEND's base URL, and the message \"<kind text> talking to <base URL>:
<cause>\" with BACKEND's key scrubbed. An HTTP-INCOMPLETE-RESPONSE's octet count
is kept."
  (let ((base (openai-base-url backend))
        (text (let ((*print-pretty* nil)) (princ-to-string cause))))
    (apply #'make-condition (%transport-error-class cause)
           :cause cause :kind kind :provider "openai" :url base
           :message (%scrub-api-key
                     backend
                     (format nil "~a talking to ~a~@[: ~a~]"
                             (or (cdr (assoc kind +%transport-kind-texts+))
                                 (string-downcase kind))
                             base
                             (and (plusp (length text)) text)))
           (and (typep cause 'http-protocol:http-incomplete-response)
                (list :octets (http-protocol:http-incomplete-response-octets cause))))))

(defun %untranslated-transport-kind (condition)
  "CONDITION's transport kind when this backend translates it, else NIL: an
ERROR signalled inside this backend's transport scope (not in the caller's
ON-PART), not already an LLM-ERROR (a translated condition, or one from a
nested backend call, is never wrapped again), with a TRANSPORT-ERROR-KIND-FOR
kind. The tests run in this order, so the report of anything else is never
rendered."
  (and *%openai-transport-scope*
       (typep condition 'error)
       (not (typep condition 'llm-error))
       (transport-error-kind-for condition)))

(deftype %untranslated-transport-condition ()
  "A condition that %UNTRANSLATED-TRANSPORT-KIND gives a kind for."
  '(satisfies %untranslated-transport-kind))

(defun %call-with-openai-transport-errors (backend thunk)
  "THUNK's values, with its transport failures typed (rules: this file's header).
A condition %UNTRANSLATED-TRANSPORT-KIND accepts unwinds (HANDLER-CASE: a
translation; http-protocol and its backends establish no restarts below this)
and its LLM-TRANSPORT-ERROR is signalled with ERROR. No other condition is
caught: it reaches outer handlers as the same object, its restarts intact."
  (handler-case (let ((*%openai-transport-scope* t))
                  (funcall thunk))
    (%untranslated-transport-condition (cause)
      (error (%openai-transport-error backend cause (transport-error-kind-for cause))))))

;;; --- context windows ---------------------------------------------------------

(defparameter +%context-window-keys+
  '("max_context_length" "loaded_context_length" "context_length" "max_model_len")
  "GET /models entry keys that hold a context window, tried in this order (LM
Studio, OpenRouter, vLLM).")

(defun %entry-context-window (entry)
  "ENTRY's (one GET /models object) first positive integer among
+%CONTEXT-WINDOW-KEYS+, or NIL."
  (and (hash-table-p entry)
       (loop for key in +%context-window-keys+
             for value = (gethash key entry)
             when (and (integerp value) (plusp value))
               return value)))

(defun %model-windows-table (entries)
  "An EQUAL table, model id → its window (or NIL), for the GET /models ENTRIES.
Total: an entry that is not an object, or has no string id, is skipped, and the
first entry wins for a duplicate id."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (entry entries table)
      (let ((id (and (hash-table-p entry) (gethash "id" entry))))
        (when (and (stringp id) (not (nth-value 1 (gethash id table))))
          (setf (gethash id table) (%entry-context-window entry)))))))

(defun %openai-model-windows (backend)
  "BACKEND's model id → window table: one GET /models on the first call, and
LIST-MODELS refreshes it. A failed fetch is cached as an empty table and
signals nothing (this file's header: any failure → NIL)."
  (let ((cached (slot-value backend 'model-windows)))
    (if (eq cached :unfetched)
        (setf (slot-value backend 'model-windows)
              (%model-windows-table
               ;; The header's "never a condition": a window is advisory.
               ;; After the mappings above every request failure is an
               ;; LLM-ERROR, except the http-protocol errors that have no
               ;; transport kind (a redirect loop, an unsupported proxy) and
               ;; so are not translated. Both families, nothing broader.
               (handler-case (%fetch-model-entries backend)
                 ((or llm-error http-protocol:http-error) () '()))))
        cached)))

(defun openai-model-context-window (backend model)
  "The context window that BACKEND's GET <base>/models reports for MODEL (an id
string or an LLM-MODEL-INFO), or NIL (rules: this file's header). One request
per backend, cached also when it fails; any failure gives NIL, never a
condition."
  (check-type backend openai-compat-backend)
  (let ((id (if (llm-model-info-p model) (llm-model-info-id model) model)))
    (and (stringp id)
         (values (gethash id (%openai-model-windows backend))))))

(defun %catalog-context-window (backend id)
  "*LLM-CATALOG*'s window for model ID (a string, or NIL) on BACKEND's provider,
with no request: the provider's LLM-MODEL-INFO for ID when it has a window,
else the provider's window, else NIL. Exported llm-protocol API only."
  (let ((provider (and *llm-catalog*
                       (find backend (list-providers *llm-catalog*)
                             :key #'llm-provider-backend :test #'eq))))
    (and provider
         (or (let ((info (and id
                              (find-if (lambda (m)
                                         (and (llm-model-info-p m)
                                              (equal id (llm-model-info-id m))))
                                       (llm-provider-models provider)))))
               (and info (llm-model-info-context-window info)))
             (llm-provider-context-window provider)))))

(defmethod context-window ((backend openai-compat-backend) model)
  "MODEL's context window on BACKEND (rules: this file's header): an
LLM-MODEL-INFO's own window, then *LLM-CATALOG*'s (%CATALOG-CONTEXT-WINDOW),
then GET /models (OPENAI-MODEL-CONTEXT-WINDOW, cached). NIL means the backend's
default model. It never calls LIST-MODELS: at most one request per backend."
  (let ((id (cond ((llm-model-info-p model) (llm-model-info-id model))
                  ((null model) (openai-default-model backend))
                  ((stringp model) model)
                  ((symbolp model) (string-downcase (symbol-name model))))))
    (or (and (llm-model-info-p model) (llm-model-info-context-window model))
        (%catalog-context-window backend id)
        (and id (openai-model-context-window backend id)))))

(defmethod (setf openai-base-url) :after (url (backend openai-compat-backend))
  "Another base URL is another server: its GET /models is fetched afresh."
  (declare (ignore url))
  (setf (slot-value backend 'model-windows) :unfetched))
