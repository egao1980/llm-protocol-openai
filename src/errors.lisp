(in-package #:llm-protocol-openai)

;;; 0.7.0 — errors and context windows (cl-stack-code H3f).
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
;;;   this adds the servers' own). Never the API key in any slot or message.
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
;;;
;;; Transport. Inside %REQUEST (and while a streamed body is read) any
;;;   condition for which LLM-PROTOCOL:TRANSPORT-ERROR-KIND-FOR returns a kind
;;;   is translated (HANDLER-CASE) into LLM-TRANSPORT-ERROR with :CAUSE the
;;;   original, :KIND, :PROVIDER "openai", :URL the base URL, and the message
;;;   "<kind text> talking to <base URL>: <cause>" (kind text: "connection
;;;   failed", "connection reset", "TLS failed", "timed out", "canceled").
;;;   HTTP-STATUS-ERRORs never reach this (statuses are values here).
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
;;;   value first (0.6), else OPENAI-MODEL-CONTEXT-WINDOW.

(defparameter +openai-context-overflow-markers+
  '("context_length_exceeded" "exceed_context_size_error"
    "exceeds the available context size" "greater than the context length"
    "context the overflows" "maximum context length")
  "Lower-case phrases OpenAI-compatible servers (OpenAI, llama.cpp, LM Studio,
vLLM) use for a prompt that does not fit.")

(defun %h3f-pending (name)
  (error "llm-protocol-openai 0.7.0: ~a is not implemented yet (H3f contract)" name))

(defun openai-context-overflow-text-p (text)
  "T when TEXT contains one of +OPENAI-CONTEXT-OVERFLOW-MARKERS+ (case-insensitive)."
  (declare (ignore text))
  (%h3f-pending 'openai-context-overflow-text-p))

(defun openai-model-context-window (backend model)
  (declare (ignore backend model))
  (%h3f-pending 'openai-model-context-window))
