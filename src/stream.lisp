(in-package #:llm-protocol-openai)

(defun %coerce-body-string (body)
  (cond
    ((stringp body) body)
    ((and (vectorp body) (not (stringp body)))
     (babel:octets-to-string body :encoding :utf-8))
    ((streamp body)
     (if (subtypep (stream-element-type body) 'character)
         (with-output-to-string (out)
           (loop for c = (read-char body nil nil)
                 while c
                 do (write-char c out)))
         (babel:octets-to-string
          (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                                 :adjustable t :fill-pointer 0)))
            (loop for b = (read-byte body nil nil)
                  while b
                  do (vector-push-extend b out))
            out)
          :encoding :utf-8)))
    (t "")))

(defun %as-sse-source (body)
  (cond
    ((streamp body) body)
    ((stringp body) (make-string-input-stream body))
    ((and (vectorp body) (not (stringp body)))
     (make-string-input-stream (babel:octets-to-string body :encoding :utf-8)))
    (t (make-string-input-stream ""))))

(defun %map-sse (body function)
  (let ((src (%as-sse-source body)))
    (unwind-protect
         (sse-protocol:map-sse-events function src)
      (unless (eq src body)
        (ignore-errors (close src)))
      (when (streamp body)
        (ignore-errors (close body))))))

(defun %sse-json (event)
  (let ((data (and event (sse-protocol:sse-event-data event))))
    (when (stringp data)
      (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) data)))
        (when (and (plusp (length trimmed))
                   (not (string-equal trimmed "[DONE]")))
          (ignore-errors (stack-json:decode trimmed)))))))

(defun %sse-done-p (event)
  "T when EVENT is the chat-completions end sentinel `data: [DONE]`."
  (let ((data (and event (sse-protocol:sse-event-data event))))
    (and (stringp data)
         (string-equal (string-trim '(#\Space #\Tab #\Newline #\Return) data)
                       "[DONE]"))))

(defun %signal-truncated-stream (response what)
  "The stream closed before WHAT arrived. Mark RESPONSE :INCOMPLETE and
signal LLM-INCOMPLETE-RESPONSE with it; USE-PARTIAL-RESPONSE returns it."
  (setf (llm-response-finish-reason response) :incomplete)
  (signal-incomplete-response
   response
   :message (format nil "stream closed before ~a (~d part~:p received)"
                    what (length (llm-response-parts response)))))

(defun %sse-event-type (event obj)
  (or (and event (sse-protocol:sse-event-type event))
      (and (hash-table-p obj) (gethash "type" obj))))

(defun %emit-part (on-part part)
  "Call the caller's ON-PART with PART, when both are given. A condition
ON-PART signals stays the caller's: it is not this backend's transport, so it
is never translated (*%OPENAI-TRANSPORT-SCOPE*, src/errors.lisp)."
  (when (and on-part part)
    (let ((*%openai-transport-scope* nil))
      (funcall on-part part))))

(defun %chat-chunk-p (obj)
  (and (hash-table-p obj) (gethash "choices" obj)))

(defun %ensure-tool-acc (table index)
  (or (gethash index table)
      (setf (gethash index table) (list :id nil :name nil :arguments ""))))

(defun %apply-chat-tool-delta (table tc)
  (when (hash-table-p tc)
    (let* ((idx (or (gethash "index" tc) 0))
           (acc (%ensure-tool-acc table idx))
           (fn (or (gethash "function" tc) tc)))
      (when (gethash "id" tc)
        (setf (getf acc :id) (gethash "id" tc)))
      (when (and (hash-table-p fn) (gethash "name" fn))
        (setf (getf acc :name) (gethash "name" fn)))
      (let ((args (and (hash-table-p fn) (gethash "arguments" fn))))
        (when (and args (not (eq args :null)))
          (setf (getf acc :arguments)
                (concatenate 'string (getf acc :arguments) (%str args)))))
      (setf (gethash idx table) acc))))

(defun %tool-acc-parts (table)
  (mapcar (lambda (k)
            (let ((acc (gethash k table)))
              (make-llm-tool-call-part
               :id (getf acc :id)
               :name (or (getf acc :name) "")
               :arguments (or (getf acc :arguments) ""))))
          (sort (loop for k being the hash-keys of table collect k) #'<)))

(defun %stream-error-event-p (event obj)
  "T when the decoded SSE object OBJ of EVENT reports an error instead of
carrying a chunk: no \"choices\", and either an \"error\" that is not JSON null
or the SSE event type `error`."
  (and (hash-table-p obj)
       (not (%chat-chunk-p obj))
       (or (let ((err (gethash "error" obj)))
             (and err (not (eq err :null))))
           (let ((type (and event (sse-protocol:sse-event-type event))))
             (and (stringp type) (string-equal type "error"))))
       t))

(defun %signal-stream-error (backend obj &key body model)
  "Signal the stream error event OBJ (a chat {\"error\": …}, a Responses `error`
or `response.failed`) through %OPENAI-HTTP-ERROR, with 0.6's RETRY and
USE-VALUE restarts (rules: src/errors.lisp). The status is the event's
\"status\", else the error's \"code\", when it is an HTTP status (an integer
from 100 to 599; a server's own error number is not); with none the mapper
uses 500. BODY is the event's raw data, MODEL the request's model."
  (let* ((err (and (hash-table-p obj)
                   (or (gethash "error" obj)
                       (let ((resp (gethash "response" obj)))
                         (and (hash-table-p resp) (gethash "error" resp))))))
         (msg (cond
                ((hash-table-p err) (or (gethash "message" err) "stream error"))
                ((stringp err) err)
                ((and (hash-table-p obj) (gethash "message" obj))
                 (gethash "message" obj))
                (t "stream error")))
         (code (if (hash-table-p err)
                   (gethash "code" err)
                   (and (hash-table-p obj) (gethash "code" obj))))
         (type (and (hash-table-p err) (gethash "type" err)))
         (status (find-if (lambda (s) (typep s '(integer 100 599)))
                          (list (and (hash-table-p obj) (gethash "status" obj)) code))))
    (%signal-openai-http-error
     (%openai-http-error backend status
                         :body body :message msg :code code :type type
                         :model model))))

(defun %stream-body (backend method path object &key model)
  "The SSE body of a streaming request. A non-2xx status signals its typed
LLM-HTTP-ERROR through %DECODE, with the Retry-After and MODEL."
  (multiple-value-bind (status body retry-after)
      (%request backend method path :object object
                :want-stream t
                :accept "text/event-stream")
    (unless (<= 200 status 299)
      (%decode backend status (%coerce-body-string body)
               :retry-after retry-after :model model))
    body))

(defun %apply-chat-chunk (obj text thinking tools on-part)
  "Apply one chat.completion.chunk. → (values finish usage model)"
  (let ((finish nil)
        (usage nil)
        (model (and (hash-table-p obj) (gethash "model" obj))))
    (when (and (hash-table-p obj) (gethash "usage" obj))
      (setf usage (%usage (gethash "usage" obj))))
    (let* ((choice (let ((cs (and (hash-table-p obj) (gethash "choices" obj))))
                     (and cs (plusp (length cs)) (elt cs 0))))
           (delta (and choice (gethash "delta" choice)))
           (reason (and choice (gethash "finish_reason" choice))))
      (when (and reason (not (eq reason :null)))
        (setf finish (%finish-reason reason)))
      (when (hash-table-p delta)
        (let ((content (gethash "content" delta))
              (think (or (gethash "reasoning_content" delta)
                         (gethash "thinking" delta)))
              (tcs (let ((v (gethash "tool_calls" delta)))
                     (if (eq v :null) nil v))))
          (when (and content (not (eq content :null))
                     (plusp (length (%str content))))
            (write-string (%str content) text)
            (%emit-part on-part (make-llm-text-part :text (%str content))))
          (when (and think (not (eq think :null))
                     (plusp (length (%str think))))
            (write-string (%str think) thinking)
            (%emit-part on-part (make-llm-thinking-part :text (%str think))))
          (dolist (tc (llm-protocol::%as-list tcs))
            (%apply-chat-tool-delta tools tc)))))
    (values finish usage model)))

(defun %assemble-chat-response (text thinking tools finish usage model
                                requested-model on-part
                                &key backend (complete t))
  "Build the LLM-RESPONSE from the accumulated stream state. When COMPLETE is
false the stream was cut short: the (possibly half-built) tool calls are kept
in the response but not emitted to ON-PART, so a streaming consumer never
sees a call it might run before the condition is handled."
  (let* ((think-s (get-output-stream-string thinking))
         (text-s (get-output-stream-string text))
         (calls (%tool-acc-parts tools)))
    (let ((promoted (%maybe-reasoning-as-content backend requested-model
                                                 text-s think-s)))
      (when (and (not (eq promoted text-s))
                 (not (%blank-text-p promoted)))
        (setf text-s (%str promoted))
        (%emit-part on-part (make-llm-text-part :text text-s))))
    (when complete
      (dolist (call calls)
        (%emit-part on-part call)))
    (make-llm-response
     :parts (append (and (plusp (length think-s))
                         (list (make-llm-thinking-part :text think-s)))
                    (and (plusp (length text-s))
                         (list (make-llm-text-part :text text-s)))
                    calls)
     :model (or model requested-model)
     :finish-reason (if calls :tool-use (or finish :stop))
     :usage usage)))

(defun %consume-chat-sse (body on-part requested-model &optional backend)
  "Assemble a streamed chat completion. Terminal signal = a finish_reason in
some chunk, or `data: [DONE]`:
  finish_reason, with or without [DONE] -> success with that reason;
  [DONE] without finish_reason          -> success, :STOP (:TOOL-USE with calls);
  neither (closed or cut stream)        -> LLM-INCOMPLETE-RESPONSE carrying the
                                           partial response (:INCOMPLETE);
                                           USE-PARTIAL-RESPONSE returns it."
  (let ((text (make-string-output-stream))
        (thinking (make-string-output-stream))
        (tools (make-hash-table :test 'eql))
        (finish nil)
        (done nil)
        (usage nil)
        (seen-model nil))
    (%map-sse
     body
     (lambda (event)
       (if (%sse-done-p event)
           (setf done t)
           (let ((obj (%sse-json event)))
             (cond ((%stream-error-event-p event obj)
                    (%signal-stream-error backend obj
                                          :body (sse-protocol:sse-event-data event)
                                          :model requested-model))
                   ((hash-table-p obj)
                    (multiple-value-bind (chunk-finish chunk-usage chunk-model)
                        (%apply-chat-chunk obj text thinking tools on-part)
                      (when chunk-finish (setf finish chunk-finish))
                      (when chunk-usage (setf usage chunk-usage))
                      (when chunk-model (setf seen-model chunk-model)))))))))
    (let* ((complete (or finish done))
           (response (%assemble-chat-response
                      text thinking tools finish usage seen-model
                      requested-model on-part
                      :backend backend :complete complete)))
      (if complete
          response
          (%signal-truncated-stream response "a finish_reason or [DONE]")))))

(defun %responses-delta (obj)
  (%str (or (and (hash-table-p obj) (gethash "delta" obj)) "")))

(defun %append-tool-args (table index &key id name delta)
  (let ((acc (or (gethash index table)
                 (setf (gethash index table)
                       (list :id id :name name :arguments "")))))
    (when id (setf (getf acc :id) id))
    (when name (setf (getf acc :name) name))
    (when (and delta (plusp (length delta)))
      (setf (getf acc :arguments)
            (concatenate 'string (getf acc :arguments) delta)))
    (setf (gethash index table) acc)
    acc))

(defun %items-from-stream (think-s text-s calls)
  (append (and (plusp (length think-s))
               (list (make-llm-reasoning-item :text think-s)))
          (and (plusp (length text-s))
               (list (make-llm-message-item
                      :role :assistant
                      :parts (list (make-llm-text-part :text text-s)))))
          (mapcar (lambda (p)
                    (make-llm-function-call-item
                     :id (llm-tool-call-part-id p)
                     :call-id (llm-tool-call-part-id p)
                     :name (llm-tool-call-part-name p)
                     :arguments (llm-tool-call-part-arguments p)))
                  calls)))

(defun %consume-responses-sse (body on-part requested-model &optional backend)
  (let ((text (make-string-output-stream))
        (thinking (make-string-output-stream))
        (tools (make-hash-table :test 'eql))
        (emitted-tools nil)
        (completed nil)
        (seen-model nil)
        (usage nil)
        (id nil)
        (finish nil)
        (done nil)
        (chat-style nil))
    (%map-sse
     body
     (lambda (event)
       (let ((obj (unless (and (%sse-done-p event) (setf done t))
                    (%sse-json event))))
         (when (hash-table-p obj)
           (cond
             ((%stream-error-event-p event obj)
              (%signal-stream-error backend obj
                                    :body (sse-protocol:sse-event-data event)
                                    :model requested-model))
             ((%chat-chunk-p obj)
              (setf chat-style t)
              (multiple-value-bind (chunk-finish chunk-usage chunk-model)
                  (%apply-chat-chunk obj text thinking tools on-part)
                (when chunk-finish (setf finish chunk-finish))
                (when chunk-usage (setf usage chunk-usage))
                (when chunk-model (setf seen-model chunk-model))))
             (t
              (let ((type (%sse-event-type event obj)))
                (when (gethash "model" obj)
                  (setf seen-model (gethash "model" obj)))
                (cond
                  ((string-equal type "response.output_text.delta")
                   (let ((delta (%responses-delta obj)))
                     (when (plusp (length delta))
                       (write-string delta text)
                       (%emit-part on-part (make-llm-text-part :text delta)))))
                  ((or (string-equal type "response.reasoning_text.delta")
                       (string-equal type "response.reasoning_summary_text.delta"))
                   (let ((delta (%responses-delta obj)))
                     (when (plusp (length delta))
                       (write-string delta thinking)
                       (%emit-part on-part (make-llm-thinking-part :text delta)))))
                  ((string-equal type "response.output_item.added")
                   (let ((item (gethash "item" obj))
                         (idx (or (gethash "output_index" obj) 0)))
                     (when (and (hash-table-p item)
                                (string-equal (gethash "type" item) "function_call"))
                       (%append-tool-args
                        tools idx
                        :id (or (gethash "call_id" item) (gethash "id" item))
                        :name (gethash "name" item)
                        :delta (let ((a (gethash "arguments" item)))
                                 (and a (not (eq a :null)) (%str a)))))))
                  ((string-equal type "response.function_call_arguments.delta")
                   (%append-tool-args
                    tools (or (gethash "output_index" obj) 0)
                    :id (or (gethash "call_id" obj) (gethash "item_id" obj))
                    :name (gethash "name" obj)
                    :delta (%responses-delta obj)))
                  ((string-equal type "response.function_call_arguments.done")
                   (let* ((idx (or (gethash "output_index" obj) 0))
                          (final (gethash "arguments" obj))
                          (acc (%append-tool-args
                                tools idx
                                :id (or (gethash "call_id" obj)
                                        (gethash "item_id" obj))
                                :name (gethash "name" obj)
                                :delta (when (and final (not (eq final :null))
                                                  (zerop (length (getf (gethash idx tools)
                                                                       :arguments))))
                                         (%str final))))
                          (part (make-llm-tool-call-part
                                 :id (getf acc :id)
                                 :name (or (getf acc :name) "")
                                 :arguments (or (getf acc :arguments) ""))))
                     (push part emitted-tools)
                     (%emit-part on-part part)))
                  ((or (string-equal type "response.completed")
                       (string-equal type "response.incomplete"))
                   (let ((resp (or (gethash "response" obj) obj)))
                     (setf completed resp)
                     (when (hash-table-p resp)
                       (setf id (or id (gethash "id" resp))
                             seen-model (or (gethash "model" resp) seen-model)
                             usage (or (%usage (gethash "usage" resp)) usage)))))
                  ((or (string-equal type "response.failed")
                       (string-equal type "error"))
                   (%signal-stream-error backend obj
                                         :body (sse-protocol:sse-event-data event)
                                         :model requested-model))))))))))
    (cond
      ((hash-table-p completed)
       (let ((r (%parse-responses completed (or seen-model requested-model))))
         (unless emitted-tools
           (dolist (part (llm-response-tool-calls r))
             (%emit-part on-part part)))
         r))
      (t
       ;; No response.completed / response.incomplete. Chat-style chunks end
       ;; on a finish_reason or [DONE] as in %CONSUME-CHAT-SSE; a native
       ;; Responses stream (or a compat one ending on [DONE]) without its
       ;; terminal event was cut short.
       (let* ((complete (or done (and chat-style finish)))
              (think-s (get-output-stream-string thinking))
              (text-s (get-output-stream-string text))
              (calls (or (reverse emitted-tools) (%tool-acc-parts tools))))
         (when (and complete (not emitted-tools))
           (dolist (call calls)
             (%emit-part on-part call)))
         (let ((response
                 (make-llm-response
                  :id id
                  :items (unless chat-style
                           (%items-from-stream think-s text-s calls))
                  :parts (append (and (plusp (length think-s))
                                      (list (make-llm-thinking-part :text think-s)))
                                 (and (plusp (length text-s))
                                      (list (make-llm-text-part :text text-s)))
                                 calls)
                  :model (or seen-model requested-model)
                  :finish-reason (if calls :tool-use (or finish :stop))
                  :usage usage)))
           (if complete
               response
               (%signal-truncated-stream
                response
                (if chat-style
                    "a finish_reason or [DONE]"
                    "response.completed")))))))))

(defmethod stream-generate ((backend openai-compat-backend) turns &key model
                            settings tools tool-choice on-part output)
  (declare (ignore output))
  (multiple-value-bind (model body)
      (%chat-completion-body backend turns :model model :settings settings
                             :tools tools :tool-choice tool-choice :stream t)
    (%call-with-openai-transport-errors
     backend
     (lambda ()
       (%consume-chat-sse (%stream-body backend :post "/chat/completions" body
                                        :model model)
                          on-part model backend)))))

(defmethod stream-respond ((backend openai-compat-backend) items &key model
                           settings tools tool-choice on-part output)
  (declare (ignore output))
  (multiple-value-bind (model body)
      (%responses-body backend items :model model :settings settings
                       :tools tools :tool-choice tool-choice :stream t)
    (%call-with-openai-transport-errors
     backend
     (lambda ()
       (%consume-responses-sse (%stream-body backend :post "/responses" body
                                             :model model)
                               on-part model backend)))))
