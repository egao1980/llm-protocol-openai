(in-package #:llm-protocol-openai/tests)

;;; Issue #16: replayed SSE bodies for streams that close with and without a
;;; terminal signal (finish_reason, [DONE], response.completed).

(defun %chunk (&key content reasoning tool-call finish usage (model "local"))
  (%sse-block
   (stack-json:encode
    (%ht "model" model
         "usage" usage
         "choices"
         (if (or content reasoning tool-call finish)
             (vector (%ht "index" 0
                          "finish_reason" (or finish :null)
                          "delta" (%ht "content" content
                                       "reasoning_content" reasoning
                                       "tool_calls" (and tool-call
                                                         (vector tool-call)))))
             (vector))))))

(defun %replay (&rest blocks)
  "A request-fn that answers every request with the SSE body BLOCKS."
  (let ((body (apply #'concatenate 'string blocks)))
    (lambda (method url &key &allow-other-keys)
      (declare (ignore method url))
      (values 200 body))))

(defun %stream-chat (request-fn &key on-part)
  (llm-protocol:stream-generate
   (llm-protocol-openai:make-openai-compat-backend :request-fn request-fn)
   "hi" :model "local" :on-part on-part))

(defun %stream-responses (request-fn &key on-part)
  (llm-protocol:stream-respond
   (llm-protocol-openai:make-openai-compat-backend :request-fn request-fn)
   "hi" :model "local" :on-part on-part))

(defun %truncated-text-body ()
  (%replay (%chunk :reasoning "think")
           (%chunk :content "Hel")
           (%chunk :content "lo")))

(defun %truncated-tool-body ()
  (%replay (%chunk :content "Calling")
           (%chunk :tool-call (%ht "index" 0 "id" "call_1" "type" "function"
                                   "function" (%ht "name" "sum" "arguments" "")))
           (%chunk :tool-call (%ht "index" 0
                                   "function" (%ht "arguments" "{\"a\":")))))

(deftest stream-truncated-mid-text-signals
  (let ((c (handler-case (progn (%stream-chat (%truncated-text-body)) nil)
             (llm-protocol:llm-incomplete-response (c) c))))
    (ok (typep c 'llm-protocol:llm-incomplete-response))
    (ok (typep c 'llm-protocol:llm-http-error))
    (ok (llm-protocol:llm-http-error-retryable-p c))
    (let ((r (llm-protocol:llm-incomplete-response-partial c)))
      (ok (eq :incomplete (llm-protocol:llm-response-finish-reason r)))
      (ok (equal "Hello" (llm-protocol:llm-response-text r)))
      (ok (equal "think" (llm-protocol:llm-response-thinking r))))))

(deftest stream-truncated-use-partial-response
  (let* ((seen nil)
         (r (handler-bind ((llm-protocol:llm-incomplete-response
                             #'llm-protocol:invoke-use-partial-response))
              (%stream-chat (%truncated-text-body)
                            :on-part (lambda (p) (push p seen))))))
    (ok (eq :incomplete (llm-protocol:llm-response-finish-reason r)))
    (ok (equal "Hello" (llm-protocol:llm-response-text r)))
    (ok (= 2 (count-if #'llm-protocol:llm-text-part-p seen)))))

(deftest stream-empty-body-is-not-a-success
  (ok (signals (%stream-chat (%replay ""))
               'llm-protocol:llm-incomplete-response))
  (let ((r (handler-bind ((llm-protocol:llm-incomplete-response
                            #'llm-protocol:invoke-use-partial-response))
             (%stream-chat (%replay "")))))
    (ok (null (llm-protocol:llm-response-parts r)))
    (ok (eq :incomplete (llm-protocol:llm-response-finish-reason r)))))

(deftest stream-truncated-mid-tool-call
  (let* ((seen nil)
         (c (handler-case
                (progn (%stream-chat (%truncated-tool-body)
                                     :on-part (lambda (p) (push p seen)))
                       nil)
              (llm-protocol:llm-incomplete-response (c) c))))
    (ok c)
    (let* ((r (llm-protocol:llm-incomplete-response-partial c))
           (call (first (llm-protocol:llm-response-tool-calls r))))
      (ok (eq :incomplete (llm-protocol:llm-response-finish-reason r)))
      (ok (equal "Calling" (llm-protocol:llm-response-text r)))
      (ok (equal "sum" (llm-protocol:llm-tool-call-part-name call)))
      (ok (equal "{\"a\":" (llm-protocol:llm-tool-call-part-arguments call))))
    ;; The half-built call never reached the streaming consumer.
    (ng (find-if #'llm-protocol:llm-tool-call-part-p seen))))

(deftest stream-truncated-retry-restart
  (let* ((n 0)
         (fn (lambda (method url &rest args)
               (incf n)
               (apply (if (= n 1)
                          (%truncated-text-body)
                          (%replay (%chunk :content "ok" :finish "stop")
                                   (%sse-block "[DONE]")))
                      method url args)))
         (r (llm-protocol:with-auto-retry (%stream-chat fn))))
    (ok (= 2 n))
    (ok (equal "ok" (llm-protocol:llm-response-text r)))
    (ok (eq :stop (llm-protocol:llm-response-finish-reason r)))))

(deftest stream-finish-reason-without-done-is-success
  (let ((r (%stream-chat
            (%replay (%chunk :content "Hi")
                     (%chunk :content "!" :finish "stop")
                     (%chunk :usage (%ht "prompt_tokens" 1 "completion_tokens" 2
                                         "total_tokens" 3))))))
    (ok (equal "Hi!" (llm-protocol:llm-response-text r)))
    (ok (eq :stop (llm-protocol:llm-response-finish-reason r)))
    (ok (= 3 (llm-protocol:llm-usage-total-tokens
              (llm-protocol:llm-response-usage r)))))
  (let ((r (%stream-chat (%replay (%chunk :content "cut" :finish "length")))))
    (ok (eq :length (llm-protocol:llm-response-finish-reason r)))))

(deftest stream-done-without-finish-reason-is-stop
  "[DONE] alone ends the stream: :STOP (or :TOOL-USE when calls arrived)."
  (let ((r (%stream-chat (%replay (%chunk :content "Hi") (%sse-block "[DONE]")))))
    (ok (equal "Hi" (llm-protocol:llm-response-text r)))
    (ok (eq :stop (llm-protocol:llm-response-finish-reason r))))
  (let ((r (%stream-chat
            (%replay (%chunk :tool-call (%ht "index" 0 "id" "c" "type" "function"
                                             "function" (%ht "name" "sum"
                                                             "arguments" "{}")))
                     (%sse-block "[DONE]")))))
    (ok (eq :tool-use (llm-protocol:llm-response-finish-reason r)))))

;;; Responses API path

(defun %responses-event (type &rest kvs)
  (%sse-block (stack-json:encode (apply #'%ht "type" type kvs)) type))

(deftest stream-respond-truncated-native
  (let ((c (handler-case
               (progn
                 (%stream-responses
                  (%replay (%responses-event "response.output_text.delta"
                                             "delta" "Hel")
                           (%responses-event "response.output_text.delta"
                                             "delta" "lo")))
                 nil)
             (llm-protocol:llm-incomplete-response (c) c))))
    (ok c)
    (let ((r (llm-protocol:llm-incomplete-response-partial c)))
      (ok (eq :incomplete (llm-protocol:llm-response-finish-reason r)))
      (ok (equal "Hello" (llm-protocol:llm-response-text r)))
      (ok (llm-protocol:llm-message-item-p
           (first (llm-protocol:llm-response-items r)))))))

(deftest stream-respond-truncated-mid-tool-call
  (let* ((seen nil)
         (r (handler-bind ((llm-protocol:llm-incomplete-response
                             #'llm-protocol:invoke-use-partial-response))
              (%stream-responses
               (%replay (%responses-event "response.output_item.added"
                                          "output_index" 0
                                          "item" (%ht "type" "function_call"
                                                      "call_id" "call_1"
                                                      "name" "sum"))
                        (%responses-event "response.function_call_arguments.delta"
                                          "output_index" 0 "delta" "{\"a\""))
               :on-part (lambda (p) (push p seen)))))
         (call (first (llm-protocol:llm-response-tool-calls r))))
    (ok (eq :incomplete (llm-protocol:llm-response-finish-reason r)))
    (ok (equal "sum" (llm-protocol:llm-tool-call-part-name call)))
    (ok (equal "{\"a\"" (llm-protocol:llm-tool-call-part-arguments call)))
    (ng (find-if #'llm-protocol:llm-tool-call-part-p seen))))

(deftest stream-respond-chat-style-terminal-rules
  (ok (signals (%stream-responses (%replay (%chunk :content "Hi")))
               'llm-protocol:llm-incomplete-response))
  (let ((r (%stream-responses (%replay (%chunk :content "Hi" :finish "stop")))))
    (ok (equal "Hi" (llm-protocol:llm-response-text r)))
    (ok (eq :stop (llm-protocol:llm-response-finish-reason r)))))
