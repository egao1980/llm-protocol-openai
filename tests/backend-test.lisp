(in-package #:llm-protocol-openai/tests)

(defun %ht (&rest kvs)
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on kvs by #'cddr
          unless (null v)
            do (setf (gethash k h) v))
    h))

(defun %sse-block (data &optional event)
  (with-output-to-string (s)
    (when event
      (format s "event: ~a~%" event))
    (format s "data: ~a~%~%" data)))

(defun %last-user-text (body)
  (let ((msgs (gethash "messages" body)))
    (when (and msgs (plusp (length msgs)))
      (gethash "content" (elt msgs (1- (length msgs)))))))

(defun %responses-input-text (body)
  (let ((input (gethash "input" body)))
    (cond
      ((stringp input) input)
      ((and input (plusp (length input)))
       (let ((last (elt input (1- (length input)))))
         (or (gethash "text" last)
             (let ((c (gethash "content" last)))
               (if (and c (plusp (length c)))
                   (gethash "text" (elt c 0))
                   "")))))
      (t ""))))

(defun %chat-sse (text &key model tools)
  (with-output-to-string (s)
    (if tools
        (progn
          (write-string
           (%sse-block
            (stack-json:encode
             (%ht "model" model
                  "choices"
                  (vector (%ht "finish_reason" :null
                               "delta"
                               (%ht "tool_calls"
                                    (vector (%ht "index" 0
                                                 "id" "call_1"
                                                 "type" "function"
                                                 "function"
                                                 (%ht "name" "sum"
                                                      "arguments" "")))))))))
           s)
          (write-string
           (%sse-block
            (stack-json:encode
             (%ht "model" model
                  "choices"
                  (vector (%ht "finish_reason" :null
                               "delta"
                               (%ht "tool_calls"
                                    (vector (%ht "index" 0
                                                 "function"
                                                 (%ht "arguments" "{\"a\":1}")))))))))
           s)
          (write-string
           (%sse-block
            (stack-json:encode
             (%ht "model" model
                  "usage" (%ht "prompt_tokens" 3 "completion_tokens" 2
                               "total_tokens" 5)
                  "choices"
                  (vector (%ht "finish_reason" "tool_calls"
                               "delta" (%ht))))))
           s))
        (let* ((full (format nil "ok:~a" text))
               (cut (min 3 (length full))))
          (write-string
           (%sse-block
            (stack-json:encode
             (%ht "model" model
                  "choices"
                  (vector (%ht "finish_reason" :null
                               "delta" (%ht "content" (subseq full 0 cut)))))))
           s)
          (when (< cut (length full))
            (write-string
             (%sse-block
              (stack-json:encode
               (%ht "model" model
                    "choices"
                    (vector (%ht "finish_reason" :null
                                 "delta" (%ht "content" (subseq full cut)))))))
             s))
          (write-string
           (%sse-block
            (stack-json:encode
             (%ht "model" model
                  "usage" (%ht "prompt_tokens" 3 "completion_tokens" 2
                               "total_tokens" 5)
                  "choices"
                  (vector (%ht "finish_reason" "stop" "delta" (%ht))))))
           s)))
    (write-string (%sse-block "[DONE]") s)))

(defun %responses-sse (text &key model tools)
  (with-output-to-string (s)
    (if tools
        (progn
          (write-string
           (%sse-block
            (stack-json:encode
             (%ht "type" "response.output_item.added"
                  "output_index" 0
                  "item" (%ht "type" "function_call"
                              "call_id" "call_1"
                              "name" "sum"
                              "arguments" "")))
            "response.output_item.added")
           s)
          (write-string
           (%sse-block
            (stack-json:encode
             (%ht "type" "response.function_call_arguments.delta"
                  "output_index" 0
                  "delta" "{\"a\":1}"))
            "response.function_call_arguments.delta")
           s)
          (write-string
           (%sse-block
            (stack-json:encode
             (%ht "type" "response.function_call_arguments.done"
                  "output_index" 0
                  "name" "sum"
                  "arguments" "{\"a\":1}"))
            "response.function_call_arguments.done")
           s)
          (write-string
           (%sse-block
            (stack-json:encode
             (%ht "type" "response.completed"
                  "response"
                  (%ht "id" "resp_1"
                       "status" "completed"
                       "model" model
                       "usage" (%ht "input_tokens" 3 "output_tokens" 2
                                    "total_tokens" 5)
                       "output"
                       (vector (%ht "type" "function_call"
                                    "call_id" "call_1"
                                    "name" "sum"
                                    "arguments" "{\"a\":1}")))))
            "response.completed")
           s))
        (let* ((full (format nil "ok:~a" text))
               (cut (min 3 (length full))))
          (write-string
           (%sse-block
            (stack-json:encode
             (%ht "type" "response.output_text.delta"
                  "delta" (subseq full 0 cut)))
            "response.output_text.delta")
           s)
          (when (< cut (length full))
            (write-string
             (%sse-block
              (stack-json:encode
               (%ht "type" "response.output_text.delta"
                    "delta" (subseq full cut)))
              "response.output_text.delta")
             s))
          (write-string
           (%sse-block
            (stack-json:encode
             (%ht "type" "response.completed"
                  "response"
                  (%ht "id" "resp_1"
                       "status" "completed"
                       "model" model
                       "usage" (%ht "input_tokens" 3 "output_tokens" 2
                                    "total_tokens" 5)
                       "output"
                       (vector (%ht "type" "message"
                                    "role" "assistant"
                                    "content"
                                    (vector (%ht "type" "output_text"
                                                 "text" full)))))))
            "response.completed")
           s)))))

(defun %fake-openai (method url &key headers content want-stream)
  (declare (ignore headers want-stream))
  (cond
    ((and (eq method :post) (search "/chat/completions" url))
     (let* ((body (stack-json:decode content))
            (text (%last-user-text body))
            (tools (gethash "tools" body))
            (model (or (gethash "model" body) "gpt-4o-mini")))
       (if (gethash "stream" body)
           (values 200 (%chat-sse text :model model :tools tools))
           (values 200
                   (stack-json:encode
                    (%ht "model" model
                         "usage" (%ht "prompt_tokens" 3 "completion_tokens" 2
                                      "total_tokens" 5)
                         "choices"
                         (vector (%ht "finish_reason" (if tools "tool_calls" "stop")
                                      "message"
                                      (%ht "role" "assistant"
                                           "content" (if tools
                                                         :null
                                                         (format nil "ok:~a" text))
                                           "tool_calls"
                                           (when tools
                                             (vector (%ht "id" "call_1"
                                                          "type" "function"
                                                          "function"
                                                          (%ht "name" "sum"
                                                               "arguments" "{\"a\":1}")))))))))))))
    ((and (eq method :post) (search "/responses" url))
     (let* ((body (stack-json:decode content))
            (text (%responses-input-text body))
            (tools (gethash "tools" body))
            (model (or (gethash "model" body) "gpt-4o-mini")))
       (if (gethash "stream" body)
           (values 200 (%responses-sse text :model model :tools tools))
           (values 200
                   (stack-json:encode
                    (%ht "id" "resp_1"
                         "status" "completed"
                         "model" model
                         "usage" (%ht "input_tokens" 3 "output_tokens" 2
                                      "total_tokens" 5)
                         "output"
                         (if tools
                             (vector (%ht "type" "function_call"
                                          "call_id" "call_1"
                                          "name" "sum"
                                          "arguments" "{\"a\":1}"))
                             (vector (%ht "type" "message"
                                          "role" "assistant"
                                          "content"
                                          (vector (%ht "type" "output_text"
                                                       "text" (format nil "ok:~a" text))))))))))))
    ((search "/models" url)
     (values 200 (stack-json:encode
                  (%ht "data" (vector (%ht "id" "local" "owned_by" "lmstudio"))))))
    ((and (eq method :post) (search "/embeddings" url))
     (let* ((body (stack-json:decode content))
            (input (gethash "input" body))
            (texts (if (stringp input) (list input) (llm-protocol::%as-list input)))
            (dim (or (gethash "dimensions" body) 4))
            (data (loop for text in texts for i from 0
                        collect (%ht "object" "embedding"
                                     "index" i
                                     "embedding" (let ((v (make-array dim)))
                                                   (dotimes (j dim)
                                                     (setf (aref v j)
                                                           (float (+ i j 1) 1d0)))
                                                   (when (plusp (length text))
                                                     (setf (aref v 0)
                                                           (float (char-code (char text 0))
                                                                  1d0)))
                                                   v)))))
       (values 200
               (stack-json:encode
                (%ht "model" (or (gethash "model" body) "text-embedding-3-small")
                     "usage" (%ht "prompt_tokens" (reduce #'+ texts :key #'length)
                                  "total_tokens" (reduce #'+ texts :key #'length))
                     "data" (map 'vector #'identity data))))))
    (t (values 404 "{}"))))

(defun %fake-openai-error (method url &key headers content want-stream)
  (declare (ignore method url headers content want-stream))
  (values 401 (stack-json:encode
               (%ht "error" (%ht "message" "invalid api key" "type" "auth")))))

(deftest openai-generate-mock-http
  (let* ((backend (llm-protocol-openai:make-openai-compat-backend
                   :base-url "http://example.invalid/v1"
                   :api-key "sk-test"
                   :request-fn #'%fake-openai))
         (r (llm-protocol:generate backend "hi" :model "local")))
    (ok (equal "ok:hi" (llm-protocol:llm-response-text r)))
    (ok (equal "local" (llm-protocol:llm-response-model r)))
    (ok (eq :stop (llm-protocol:llm-response-finish-reason r)))
    (ok (= 5 (llm-protocol:llm-usage-total-tokens (llm-protocol:llm-response-usage r))))))

(deftest openai-settings-on-wire
  (let ((seen nil))
    (flet ((capture (method url &key headers content &allow-other-keys)
             (declare (ignore method url headers))
             (setf seen (stack-json:decode content))
             (%fake-openai :post "http://x/chat/completions" :content content)))
      (llm-protocol:generate
       (llm-protocol-openai:make-openai-compat-backend :request-fn #'capture)
       "hi"
       :settings '(:temperature 0 :max-tokens 16))
      (ok (zerop (gethash "temperature" seen)))
      (ok (= 16 (gethash "max_tokens" seen))))))

(deftest openai-output-schema-on-wire
  (let ((seen nil)
        (schema (let ((h (make-hash-table :test 'equal)))
                  (setf (gethash "type" h) "object")
                  h)))
    (flet ((capture (method url &key headers content &allow-other-keys)
             (declare (ignore method url headers))
             (setf seen (stack-json:decode content))
             (%fake-openai :post "http://x/chat/completions" :content content)))
      (llm-protocol:generate
       (llm-protocol-openai:make-openai-compat-backend :request-fn #'capture)
       "hi"
       :output schema)
      (let ((fmt (gethash "response_format" seen)))
        (ok (equal "json_schema" (gethash "type" fmt)))
        (ok (equal "object" (gethash "type" (gethash "schema"
                                                     (gethash "json_schema" fmt)))))))))

(deftest openai-tools-mock-http
  (let* ((backend (llm-protocol-openai:make-openai-compat-backend
                   :request-fn #'%fake-openai))
         (r (llm-protocol:generate backend "add"
                                   :tools (list (llm-protocol:make-llm-tool :name "sum")))))
    (ok (eq :tool-use (llm-protocol:llm-response-finish-reason r)))
    (ok (equal "sum" (llm-protocol:llm-tool-call-part-name
                      (first (llm-protocol:llm-response-tool-calls r)))))))

(deftest openai-list-models-mock-http
  (let ((models (llm-protocol:list-models
                 (llm-protocol-openai:make-openai-compat-backend
                  :request-fn #'%fake-openai))))
    (ok (equal "local" (llm-protocol:llm-model-info-id (first models))))
    (ok (equal "lmstudio" (llm-protocol:llm-model-info-owned-by (first models))))))

(deftest openai-http-error
  (ok (signals (llm-protocol:generate
                (llm-protocol-openai:make-openai-compat-backend
                 :request-fn #'%fake-openai-error)
                "hi")
               'llm-protocol:llm-http-error)))

(defun %fake-openai-429 (method url &key headers content want-stream)
  (declare (ignore method url headers content want-stream))
  (values 429 (stack-json:encode
               (%ht "error" (%ht "message" "rate limited" "type" "rate")))))

(deftest openai-http-retryable-slot
  (handler-case
      (llm-protocol:generate
       (llm-protocol-openai:make-openai-compat-backend
        :request-fn #'%fake-openai-429)
       "hi")
    (llm-protocol:llm-http-error (c)
      (ok (eql 429 (llm-protocol:llm-http-error-status c)))
      (ok (llm-protocol:llm-http-error-retryable-p c))))
  (handler-case
      (llm-protocol:generate
       (llm-protocol-openai:make-openai-compat-backend
        :request-fn #'%fake-openai-error)
       "hi")
    (llm-protocol:llm-http-error (c)
      (ok (eql 401 (llm-protocol:llm-http-error-status c)))
      (ng (llm-protocol:llm-http-error-retryable-p c)))))

(deftest openai-stream-generate-mock
  (let* ((seen nil)
         (backend (llm-protocol-openai:make-openai-compat-backend
                   :request-fn #'%fake-openai))
         (r (llm-protocol:stream-generate
             backend "hi" :model "local"
             :on-part (lambda (p) (push p seen)))))
    (ok (equal "ok:hi" (llm-protocol:llm-response-text r)))
    (ok (equal "local" (llm-protocol:llm-response-model r)))
    (ok (eq :stop (llm-protocol:llm-response-finish-reason r)))
    (ok (= 5 (llm-protocol:llm-usage-total-tokens (llm-protocol:llm-response-usage r))))
    (ok (>= (length (remove-if-not #'llm-protocol:llm-text-part-p seen)) 2))))

(deftest openai-stream-generate-tools
  (let* ((backend (llm-protocol-openai:make-openai-compat-backend
                   :request-fn #'%fake-openai))
         (r (llm-protocol:stream-generate
             backend "add"
             :tools (list (llm-protocol:make-llm-tool :name "sum")))))
    (ok (eq :tool-use (llm-protocol:llm-response-finish-reason r)))
    (ok (equal "sum" (llm-protocol:llm-tool-call-part-name
                      (first (llm-protocol:llm-response-tool-calls r)))))
    (ok (equal "{\"a\":1}" (llm-protocol:llm-tool-call-part-arguments
                            (first (llm-protocol:llm-response-tool-calls r)))))))

(deftest openai-stream-generate-wire
  (let ((seen nil))
    (flet ((capture (method url &key headers content &allow-other-keys)
             (declare (ignore method headers))
             (ok (search "/chat/completions" url))
             (setf seen (stack-json:decode content))
             (%fake-openai :post url :content content)))
      (llm-protocol:stream-generate
       (llm-protocol-openai:make-openai-compat-backend :request-fn #'capture)
       "hi")
      (ok (eq t (gethash "stream" seen)))
      (ok (eq t (gethash "include_usage" (gethash "stream_options" seen)))))))

(deftest openai-stream-respond-mock
  (let* ((seen nil)
         (backend (llm-protocol-openai:make-openai-compat-backend
                   :request-fn #'%fake-openai))
         (r (llm-protocol:stream-respond
             backend "hi" :model "local"
             :on-part (lambda (p) (push p seen)))))
    (ok (equal "ok:hi" (llm-protocol:llm-response-text r)))
    (ok (equal "resp_1" (llm-protocol:llm-response-id r)))
    (ok (llm-protocol:llm-message-item-p (first (llm-protocol:llm-response-items r))))
    (ok (eq :stop (llm-protocol:llm-response-finish-reason r)))
    (ok (>= (length (remove-if-not #'llm-protocol:llm-text-part-p seen)) 2))))

(deftest openai-stream-respond-tools
  (let* ((seen nil)
         (backend (llm-protocol-openai:make-openai-compat-backend
                   :request-fn #'%fake-openai))
         (r (llm-protocol:stream-respond
             backend "add"
             :tools (list (llm-protocol:make-llm-tool :name "sum"))
             :on-part (lambda (p) (push p seen)))))
    (ok (eq :tool-use (llm-protocol:llm-response-finish-reason r)))
    (ok (llm-protocol:llm-function-call-item-p
         (first (llm-protocol:llm-response-items r))))
    (ok (equal "sum" (llm-protocol:llm-tool-call-part-name
                      (first (llm-protocol:llm-response-tool-calls r)))))
    (ok (find-if #'llm-protocol:llm-tool-call-part-p seen))))

(deftest openai-stream-respond-wire
  (let ((seen nil))
    (flet ((capture (method url &key headers content &allow-other-keys)
             (declare (ignore method headers))
             (ok (search "/responses" url))
             (setf seen (stack-json:decode content))
             (%fake-openai :post url :content content)))
      (llm-protocol:stream-respond
       (llm-protocol-openai:make-openai-compat-backend :request-fn #'capture)
       "hi"
       :settings '(:temperature 0 :max-tokens 16))
      (ok (eq t (gethash "stream" seen)))
      (ok (zerop (gethash "temperature" seen)))
      (ok (= 16 (gethash "max_output_tokens" seen)))
      (ok (equal "hi" (gethash "input" seen))))))

(deftest openai-stream-respond-reasoning
  (flet ((reasoning (method url &key headers content &allow-other-keys)
           (declare (ignore method url headers content))
           (values 200
                   (concatenate
                    'string
                    (%sse-block
                     (stack-json:encode
                      (%ht "type" "response.reasoning_summary_text.delta"
                           "delta" "scratch"))
                     "response.reasoning_summary_text.delta")
                    (%sse-block
                     (stack-json:encode
                      (%ht "type" "response.output_text.delta" "delta" "ok"))
                     "response.output_text.delta")
                    (%sse-block
                     (stack-json:encode
                      (%ht "type" "response.completed"
                           "response"
                           (%ht "id" "resp_r"
                                "status" "completed"
                                "model" "local"
                                "output"
                                (vector
                                 (%ht "type" "reasoning"
                                      "summary"
                                      (vector (%ht "type" "summary_text"
                                                   "text" "scratch")))
                                 (%ht "type" "message"
                                      "role" "assistant"
                                      "content"
                                      (vector (%ht "type" "output_text"
                                                   "text" "ok")))))))
                     "response.completed")))))
    (let* ((seen nil)
           (r (llm-protocol:stream-respond
               (llm-protocol-openai:make-openai-compat-backend :request-fn #'reasoning)
               "hi"
               :on-part (lambda (p) (push p seen)))))
      (ok (equal "ok" (llm-protocol:llm-response-text r)))
      (ok (equal "scratch" (llm-protocol:llm-response-thinking r)))
      (ok (find-if #'llm-protocol:llm-thinking-part-p seen))
      (ok (find-if #'llm-protocol:llm-reasoning-item-p
                   (llm-protocol:llm-response-items r))))))

(deftest openai-stream-respond-failed
  (flet ((failing (method url &key headers content &allow-other-keys)
           (declare (ignore method url headers content))
           (values 200
                   (%sse-block
                    (stack-json:encode
                     (%ht "type" "response.failed"
                          "response"
                          (%ht "id" "resp_x"
                               "status" "failed"
                               "error" (%ht "message" "boom"))))
                    "response.failed"))))
    (ok (signals (llm-protocol:stream-respond
                  (llm-protocol-openai:make-openai-compat-backend :request-fn #'failing)
                  "hi")
                 'llm-protocol:llm-http-error))))

(deftest openai-stream-respond-chat-chunk-compat
  "LM Studio / proxies sometimes stream /responses as chat.completion.chunk."
  (flet ((compat (method url &key headers content &allow-other-keys)
           (declare (ignore method headers content))
           (ok (search "/responses" url))
           (values 200 (%chat-sse "hi" :model "local"))))
    (let ((r (llm-protocol:stream-respond
              (llm-protocol-openai:make-openai-compat-backend :request-fn #'compat)
              "hi" :model "local")))
      (ok (equal "ok:hi" (llm-protocol:llm-response-text r)))
      (ok (eq :stop (llm-protocol:llm-response-finish-reason r))))))

(deftest openai-respond-mock-http
  (let* ((backend (llm-protocol-openai:make-openai-compat-backend
                   :request-fn #'%fake-openai))
         (r (llm-protocol:respond backend "hi" :model "local")))
    (ok (equal "ok:hi" (llm-protocol:llm-response-text r)))
    (ok (equal "resp_1" (llm-protocol:llm-response-id r)))
    (ok (llm-protocol:llm-message-item-p (first (llm-protocol:llm-response-items r))))
    (ok (eq :stop (llm-protocol:llm-response-finish-reason r)))))

(deftest openai-respond-settings-on-wire
  (let ((seen nil))
    (flet ((capture (method url &key headers content &allow-other-keys)
             (declare (ignore method headers))
             (ok (search "/responses" url))
             (setf seen (stack-json:decode content))
             (%fake-openai :post url :content content)))
      (llm-protocol:respond
       (llm-protocol-openai:make-openai-compat-backend :request-fn #'capture)
       "hi"
       :settings '(:temperature 0 :max-tokens 16))
      (ok (zerop (gethash "temperature" seen)))
      (ok (= 16 (gethash "max_output_tokens" seen)))
      (ok (equal "hi" (gethash "input" seen))))))

(deftest openai-respond-settings-extra-on-wire
  (let ((seen nil))
    (flet ((capture (method url &key headers content &allow-other-keys)
             (declare (ignore method headers))
             (ok (search "/responses" url))
             (setf seen (stack-json:decode content))
             (%fake-openai :post url :content content)))
      (llm-protocol:respond
       (llm-protocol-openai:make-openai-compat-backend :request-fn #'capture)
       "hi"
       :settings '(:temperature 0
                   :extra (:previous-response-id "resp_prev"
                           :instructions "be brief"
                           :reasoning (:effort :medium :summary :auto)
                           :store nil
                           :conversation (:id "conv_1"))))
      (ok (equal "resp_prev" (gethash "previous_response_id" seen)))
      (ok (equal "be brief" (gethash "instructions" seen)))
      (ok (equal "medium" (gethash "effort" (gethash "reasoning" seen))))
      (ok (equal "auto" (gethash "summary" (gethash "reasoning" seen))))
      (ok (nth-value 1 (gethash "store" seen)))
      (ok (null (gethash "store" seen)))
      (ok (equal "conv_1" (gethash "id" (gethash "conversation" seen))))
      (ok (zerop (gethash "temperature" seen))))))

(deftest openai-settings-extra-first-class-wins
  (let ((seen nil))
    (flet ((capture (method url &key headers content &allow-other-keys)
             (declare (ignore method url headers))
             (setf seen (stack-json:decode content))
             (%fake-openai :post "http://x/chat/completions" :content content)))
      (llm-protocol:generate
       (llm-protocol-openai:make-openai-compat-backend :request-fn #'capture)
       "hi"
       :settings '(:temperature 0 :extra (:temperature 1 :seed 7)))
      (ok (zerop (gethash "temperature" seen)))
      (ok (= 7 (gethash "seed" seen))))))

(deftest openai-stream-respond-settings-extra
  (let ((seen nil))
    (flet ((capture (method url &key headers content &allow-other-keys)
             (declare (ignore method headers))
             (ok (search "/responses" url))
             (setf seen (stack-json:decode content))
             (%fake-openai :post url :content content)))
      (llm-protocol:stream-respond
       (llm-protocol-openai:make-openai-compat-backend :request-fn #'capture)
       "hi"
       :settings '(:extra (:previous-response-id "resp_prev" :instructions "x")))
      (ok (eq t (gethash "stream" seen)))
      (ok (equal "resp_prev" (gethash "previous_response_id" seen)))
      (ok (equal "x" (gethash "instructions" seen))))))

(deftest openai-respond-tools
  (let* ((backend (llm-protocol-openai:make-openai-compat-backend
                   :request-fn #'%fake-openai))
         (r (llm-protocol:respond backend "add"
                                  :tools (list (llm-protocol:make-llm-tool :name "sum")))))
    (ok (eq :tool-use (llm-protocol:llm-response-finish-reason r)))
    (ok (llm-protocol:llm-function-call-item-p
         (first (llm-protocol:llm-response-items r))))
    (ok (equal "sum" (llm-protocol:llm-tool-call-part-name
                      (first (llm-protocol:llm-response-tool-calls r)))))))

(deftest openai-catalogue
  (let ((cat (llm-protocol:make-llm-catalogue
              (llm-protocol-openai:make-openai-compat-backend))))
    (ok (capability-protocol:capability-supported-p cat :llm-vision))
    (ok (capability-protocol:capability-supported-p cat :llm-structured-output))
    (ok (capability-protocol:capability-supported-p cat :llm-responses))
    (ng (capability-protocol:capability-supported-p cat :llm-video))
    (ok (llm-protocol:backend-supports-p
         (llm-protocol-openai:make-openai-compat-backend) :stream))
    (ok (llm-protocol:backend-supports-p
         (llm-protocol-openai:make-openai-compat-backend) :responses))
    (ok (llm-protocol:backend-supports-p
         (llm-protocol-openai:make-openai-compat-backend) :embeddings))
    (ok (capability-protocol:capability-supported-p cat :llm-embeddings))
    (let ((gen (capability-protocol:get-capability cat :llm-generation)))
      (ok (find 'capability-protocol:stream-complete
                (capability-protocol:capability-operations gen)
                :key #'capability-protocol:capability-operation-name)))))

(deftest openai-embed-one
  (let* ((b (llm-protocol-openai:make-openai-compat-backend
             :request-fn #'%fake-openai
             :embedding-model "text-embedding-3-small"))
         (r (llm-protocol:embed b "ab" :dimensions 4)))
    (ok (llm-protocol:llm-embed-result-p r))
    (ok (equal "text-embedding-3-small" (llm-protocol:llm-embed-result-model r)))
    (let ((v (llm-protocol:llm-embedding-vector
              (first (llm-protocol:llm-embed-result-embeddings r)))))
      (ok (= 4 (length v)))
      (ok (= (float (char-code #\a) 1f0) (aref v 0))))
    (ok (= 2 (llm-protocol:llm-usage-total-tokens
              (llm-protocol:llm-embed-result-usage r))))))

(deftest openai-embed-many-and-path
  (let ((seen-url nil)
        (seen-body nil))
    (flet ((capture (method url &key headers content &allow-other-keys)
             (declare (ignore method headers))
             (setf seen-url url seen-body (stack-json:decode content))
             (%fake-openai :post url :content content)))
      (let* ((b (llm-protocol-openai:make-openai-compat-backend
                 :base-url "http://example.invalid/v1"
                 :request-fn #'capture))
             (r (llm-protocol:embed b '("one" "two") :model "emb-2" :dimensions 3))
             (embs (llm-protocol:llm-embed-result-embeddings r)))
        (ok (search "/embeddings" seen-url))
        (ok (equal "emb-2" (gethash "model" seen-body)))
        (ok (= 2 (length (gethash "input" seen-body))))
        (ok (= 3 (gethash "dimensions" seen-body)))
        (ok (equal "float" (gethash "encoding_format" seen-body)))
        (ok (= 2 (length embs)))
        (ok (zerop (llm-protocol:llm-embedding-index (first embs))))
        (ok (= 1 (llm-protocol:llm-embedding-index (second embs))))))))

(deftest openai-embed-query
  (let ((v (llm-protocol:embed-query
            (llm-protocol-openai:make-openai-compat-backend
             :request-fn #'%fake-openai)
            "z" :dimensions 2)))
    (ok (vectorp v))
    (ok (= (float (char-code #\z) 1f0) (aref v 0)))))

(deftest openai-embed-rejects-base64
  (ok (signals (llm-protocol:embed
                (llm-protocol-openai:make-openai-compat-backend
                 :request-fn #'%fake-openai)
                "hi" :encoding-format :base64)
               'llm-protocol:llm-unsupported)))

(deftest openai-embed-429
  (ok (signals (llm-protocol:embed
                (llm-protocol-openai:make-openai-compat-backend
                 :request-fn #'%fake-openai-429)
                "hi")
               'llm-protocol:llm-http-error)))

(defmacro %with-async-http (&body body)
  "Bind http-backend-async × libuv like http-parity WITH-PARITY."
  `(let* ((eb (event-backend-libuv:make-libuv-backend))
          (el (event-protocol:make-event-loop eb))
          (http-backend-async:*event-backend-maker* (lambda () eb)))
     (event-protocol:with-event-backend (eb)
       (event-protocol:with-event-loop-var (el)
         (let ((http-protocol:*http-backend* (http-backend-async:make-async-backend)))
           ,@body)))))

(defun %read-crlf-line (stream)
  (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0)))
    (loop for b = (read-byte stream nil nil)
          while b
          do (vector-push-extend b out)
             (when (and (>= (length out) 2)
                        (= (aref out (- (length out) 2)) 13)
                        (= (aref out (- (length out) 1)) 10))
               (return)))
    (when (plusp (length out))
      (string-right-trim '(#\Return #\Newline)
                         (babel:octets-to-string out :encoding :utf-8 :errorp nil)))))

(defun %handle-openai-http (client)
  (let* ((stream (usocket:socket-stream client))
         (req-line (%read-crlf-line stream))
         (content-length 0))
    (unless req-line
      (return-from %handle-openai-http))
    (loop for line = (%read-crlf-line stream)
          while (and line (plusp (length line)))
          do (let ((colon (position #\: line)))
               (when colon
                 (let ((name (string-downcase (subseq line 0 colon)))
                       (val (string-trim '(#\Space #\Tab) (subseq line (1+ colon)))))
                   (when (string= name "content-length")
                     (setf content-length (or (parse-integer val :junk-allowed t) 0)))))))
    (let* ((parts (uiop:split-string req-line :separator " "))
           (method (intern (string-upcase (first parts)) :keyword))
           (target (second parts))
           (body (when (plusp content-length)
                   (let ((buf (make-array content-length
                                          :element-type '(unsigned-byte 8))))
                     (read-sequence buf stream)
                     (babel:octets-to-string buf :encoding :utf-8))))
           (url (format nil "http://fixture~a" target)))
      (multiple-value-bind (status payload)
          (%fake-openai method url :headers nil :content (or body "{}"))
        (let* ((octets (babel:string-to-octets payload :encoding :utf-8))
               (sse-p (or (eql 0 (search "data:" payload))
                          (eql 0 (search "event:" payload))))
               (head (babel:string-to-octets
                      (format nil
                              "HTTP/1.1 ~a OK~C~CContent-Type: ~a~C~CContent-Length: ~a~C~CConnection: close~C~C~C~C"
                              status #\Return #\Newline
                              (if sse-p "text/event-stream" "application/json")
                              #\Return #\Newline
                              (length octets) #\Return #\Newline #\Return #\Newline
                              #\Return #\Newline))))
          (write-sequence head stream)
          (write-sequence octets stream)
          (force-output stream))))))

(defun %with-fake-openai-http (fn)
  (let* ((server (usocket:socket-listen "127.0.0.1" 0
                                       :reuseaddress t
                                       :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port server))
         (stop nil)
         (thread (bt:make-thread
                  (lambda ()
                    (loop until stop
                          do (handler-case
                                 (let ((c (usocket:socket-accept
                                           server
                                           :element-type '(unsigned-byte 8))))
                                   (unwind-protect (%handle-openai-http c)
                                     (ignore-errors (usocket:socket-close c))))
                               (error () nil))))
                  :name "llm-openai-fixture")))
    (unwind-protect
         (funcall fn (format nil "http://127.0.0.1:~a/v1" port))
      (setf stop t)
      (ignore-errors (usocket:socket-close server))
      (when (bt:thread-alive-p thread)
        (ignore-errors (bt:destroy-thread thread))))))

(deftest openai-async-http-fixture
  (%with-fake-openai-http
   (lambda (base)
     (%with-async-http
       (let* ((b (llm-protocol-openai:make-openai-compat-backend
                  :base-url base :api-key "sk-test" :default-model "local"))
              (gen (llm-protocol:generate b "hi" :model "local"))
              (res (llm-protocol:respond b "hi"))
              (models (llm-protocol:list-models b)))
         (ok (equal "ok:hi" (llm-protocol:llm-response-text gen)))
         (ok (equal "ok:hi" (llm-protocol:llm-response-text res)))
         (ok (equal "resp_1" (llm-protocol:llm-response-id res)))
         (ok (equal "local" (llm-protocol:llm-model-info-id (first models))))
         (let ((sg (llm-protocol:stream-generate b "hi" :model "local"))
               (sr (llm-protocol:stream-respond b "hi")))
           (ok (equal "ok:hi" (llm-protocol:llm-response-text sg)))
           (ok (equal "ok:hi" (llm-protocol:llm-response-text sr)))
           (ok (equal "resp_1" (llm-protocol:llm-response-id sr)))))))))

(defun %live-p ()
  (let ((v (uiop:getenv "LLM_OPENAI_LIVE")))
    (and v (plusp (length v)))))

(defun %live-ok (r)
  (and (llm-protocol:llm-response-p r)
       (or (plusp (length (or (llm-protocol:llm-response-text r) "")))
           (plusp (length (or (llm-protocol:llm-response-thinking r) ""))))))

(deftest openai-live-generate
  (if (%live-p)
      (%with-async-http
        (let ((r (llm-protocol:generate
                  (llm-protocol-openai:make-openai-compat-backend)
                  "Reply with the single word pong and nothing else."
                  :settings '(:temperature 0 :max-tokens 256))))
          (ok (%live-ok r))))
      (skip "set LLM_OPENAI_LIVE=1 for a live OpenAI-compat call")))

(deftest openai-live-respond
  (if (%live-p)
      (%with-async-http
        (let ((r (llm-protocol:respond
                  (llm-protocol-openai:make-openai-compat-backend)
                  "Reply with the single word pong and nothing else."
                  :settings '(:temperature 0 :max-tokens 256))))
          (ok (%live-ok r))
          (ok (plusp (length (or (llm-protocol:llm-response-id r) ""))))))
      (skip "set LLM_OPENAI_LIVE=1 for a live Responses call")))

(deftest openai-live-stream-generate
  (if (%live-p)
      (%with-async-http
        (let* ((seen nil)
               (r (llm-protocol:stream-generate
                   (llm-protocol-openai:make-openai-compat-backend)
                   "Reply with the single word pong and nothing else."
                   :settings '(:temperature 0 :max-tokens 256)
                   :on-part (lambda (p) (push p seen)))))
          (ok (%live-ok r))
          (ok (plusp (length seen)))))
      (skip "set LLM_OPENAI_LIVE=1 for a live streaming chat call")))

(deftest openai-live-stream-respond
  (if (%live-p)
      (%with-async-http
        (let* ((seen nil)
               (r (llm-protocol:stream-respond
                   (llm-protocol-openai:make-openai-compat-backend)
                   "Reply with the single word pong and nothing else."
                   :settings '(:temperature 0 :max-tokens 256)
                   :on-part (lambda (p) (push p seen)))))
          (ok (%live-ok r))
          (ok (plusp (length (or (llm-protocol:llm-response-id r) ""))))))
      (skip "set LLM_OPENAI_LIVE=1 for a live streaming Responses call")))

(deftest encode-image-part-url
  (let ((h (llm-protocol-openai:encode-image-part
            (llm-protocol:make-llm-image-part :url "https://ex.test/a.png"))))
    (ok (equal "image_url" (gethash "type" h)))
    (ok (equal "https://ex.test/a.png"
               (gethash "url" (gethash "image_url" h))))))

(deftest encode-image-part-base64-octets
  (let* ((octets (make-array 3 :element-type '(unsigned-byte 8)
                             :initial-contents '(1 2 3)))
         (h (llm-protocol-openai:encode-image-part
             (llm-protocol:make-llm-image-part
              :data octets :media-type "image/png")))
         (url (gethash "url" (gethash "image_url" h))))
    (ok (equal "image_url" (gethash "type" h)))
    (ok (equal "data:image/png;base64,AQID" url))))

(deftest encode-image-part-responses-style
  (let ((h (llm-protocol-openai:encode-image-part
            (llm-protocol:make-llm-image-part :url "https://ex.test/b.png")
            :style :responses)))
    (ok (equal "input_image" (gethash "type" h)))
    (ok (equal "https://ex.test/b.png" (gethash "image_url" h)))))

(deftest openai-image-part-on-wire
  (let ((seen nil))
    (flet ((capture (method url &key headers content &allow-other-keys)
             (declare (ignore method url headers))
             (setf seen (stack-json:decode content))
             (%fake-openai :post "http://x/chat/completions" :content content)))
      (llm-protocol:generate
       (llm-protocol-openai:make-openai-compat-backend :request-fn #'capture)
       (llm-protocol:make-llm-turn
        :role :user
        :parts (list (llm-protocol:make-llm-text-part :text "see")
                     (llm-protocol:make-llm-image-part
                      :url "https://ex.test/a.png"))))
      (let* ((msgs (gethash "messages" seen))
             (content (gethash "content" (elt msgs 0)))
             (img (elt content 1)))
        (ok (vectorp content))
        (ok (equal "image_url" (gethash "type" img)))
        (ok (equal "https://ex.test/a.png"
                   (gethash "url" (gethash "image_url" img))))))))
