(in-package #:llm-protocol-openai/tests)

;;; 0.7.0: errors, Retry-After, transport, context windows
;;; (src/errors.lisp). Every request goes through a :REQUEST-FN; no network.

(defun %err-fn (status message &key retry-after code type)
  (lambda (method url &key &allow-other-keys)
    (declare (ignore method url))
    (values status
            (stack-json:encode
             (%ht "error" (%ht "message" message "code" code "type" type)))
            retry-after)))

(defun %gen (fn &key (model "qwen"))
  (llm-protocol:generate
   (llm-protocol-openai:make-openai-compat-backend
    :request-fn fn :base-url "http://127.0.0.1:1234/v1" :api-key "sk-secret-123")
   "hi" :model model))

(defun %caught (thunk)
  (handler-case (progn (funcall thunk) nil)
    (llm-protocol:llm-error (c) c)))

(defparameter +openai-error-rows+
  '((429 "Rate limit reached" nil nil llm-protocol:llm-rate-limited)
    (503 "Service Unavailable" nil nil llm-protocol:llm-overloaded)
    (500 "The server is overloaded" nil nil llm-protocol:llm-overloaded)
    (401 "Incorrect API key provided" "invalid_api_key" nil llm-protocol:llm-auth-error)
    (403 "forbidden" nil nil llm-protocol:llm-auth-error)
    (404 "The model `nope` does not exist" "model_not_found" nil llm-protocol:llm-not-found)
    (400 "This model's maximum context length is 8192 tokens. However, your messages resulted in 9001 tokens."
     "context_length_exceeded" "invalid_request_error" llm-protocol:llm-context-overflow)
    (400 "the request exceeds the available context size, try increasing it"
     nil "exceed_context_size_error" llm-protocol:llm-context-overflow)
    (400 "Trying to keep the first 9000 tokens when context the overflows. However, the model is loaded with context length of only 8192 tokens"
     nil nil llm-protocol:llm-context-overflow)
    (400 "bad request: tools[0] invalid" nil nil llm-protocol:llm-http-error)
    (502 "bad gateway" nil nil llm-protocol:llm-http-error))
  "(status message code type expected-class); exact class, not a subclass.")

(deftest openai-error-classes
  (dolist (row +openai-error-rows+)
    (destructuring-bind (status message code type class) row
      (let ((c (%caught (lambda () (%gen (%err-fn status message :code code :type type))))))
        (ok (and c (eq class (type-of c)))
            (format nil "~a ~s → ~a (got ~a)" status message class (and c (type-of c))))))))

(deftest openai-error-names-provider-url-model-never-the-key
  (let ((c (%caught (lambda () (%gen (%err-fn 401 "Incorrect API key provided: sk-secr***") :model "gpt-x")))))
    (ok (typep c 'llm-protocol:llm-auth-error))
    (ok (equal "openai" (llm-protocol:llm-error-provider c)))
    (ok (equal "http://127.0.0.1:1234/v1" (llm-protocol:llm-error-url c)))
    (ok (equal "gpt-x" (llm-protocol:llm-error-model c)))
    (ng (search "sk-secret-123" (princ-to-string c)))
    (ng (search "sk-secret-123" (or (llm-protocol:llm-http-error-body c) "")))))

(deftest openai-retry-after-header
  (let ((c (%caught (lambda () (%gen (%err-fn 429 "slow down" :retry-after "12"))))))
    (ok (typep c 'llm-protocol:llm-rate-limited))
    (ok (eql 12 (llm-protocol:llm-http-error-retry-after c))))
  (let ((c (%caught (lambda () (%gen (%err-fn 429 "slow down" :retry-after 1.5))))))
    (ok (eql 1.5 (llm-protocol:llm-http-error-retry-after c))))
  (let ((c (%caught (lambda () (%gen (%err-fn 429 "slow down"))))))
    (ok (null (llm-protocol:llm-http-error-retry-after c)))))

(deftest openai-errors-keep-restarts
  (let ((restarts nil))
    (ok (equal "supplied"
               (handler-bind ((llm-protocol:llm-rate-limited
                                (lambda (c)
                                  (setf restarts
                                        (list (and (find-restart 'llm-protocol-openai::retry c) t)
                                              (and (find-restart 'use-value c) t)))
                                  (use-value (%ht "choices"
                                                  (vector (%ht "message" (%ht "content" "supplied")
                                                               "finish_reason" "stop")))
                                             c))))
                 (llm-protocol:llm-response-text
                  (%gen (%err-fn 429 "rl"))))))
    (ok (equal '(t t) restarts))))

(defun %raise-fn (condition-type message)
  (lambda (method url &key &allow-other-keys)
    (declare (ignore method url))
    (error condition-type :message message)))

(deftest openai-transport-errors
  (dolist (row '((http-protocol:http-connection-error "Connection refused" :connect)
                 (http-protocol:http-connection-error "connection reset by peer" :reset)
                 (http-protocol:http-timeout-error "sync await timed out" :timeout)
                 (http-protocol:http-tls-error "certificate verify failed" :tls)
                 (http-protocol:http-protocol-error "incomplete HTTP response" :reset)))
    (destructuring-bind (type message kind) row
      (let ((c (%caught (lambda () (%gen (%raise-fn type message))))))
        (ok (typep c 'llm-protocol:llm-transport-error) (format nil "~a → transport" type))
        (when (typep c 'llm-protocol:llm-transport-error)
          (ok (eq kind (llm-protocol:llm-transport-error-kind c)) (format nil "~s → ~s" message kind))
          (ok (typep (llm-protocol:llm-transport-error-cause c) type))
          (ok (equal "openai" (llm-protocol:llm-transport-error-provider c)))
          (ok (search "http://127.0.0.1:1234/v1" (princ-to-string c)))
          (ok (eq :transient (llm-protocol:llm-retry-class c))))))))

(deftest openai-transport-errors-in-stream-generate
  (let ((c (handler-case
               (progn (llm-protocol:stream-generate
                       (llm-protocol-openai:make-openai-compat-backend
                        :request-fn (%raise-fn 'http-protocol:http-connection-error "Connection refused"))
                       "hi" :on-part (lambda (p) (declare (ignore p))))
                      nil)
             (llm-protocol:llm-error (c) c))))
    (ok (typep c 'llm-protocol:llm-transport-error))))

(deftest openai-other-errors-pass-through
  (ok (signals (%gen (lambda (method url &key &allow-other-keys)
                       (declare (ignore method url))
                       (error "programming bug")))
               'simple-error)))

(defun %stream-error-fn (obj)
  (lambda (method url &key &allow-other-keys)
    (declare (ignore method url))
    (values 200 (%sse-block (stack-json:encode obj)))))

(deftest openai-stream-error-events
  (flet ((fail-with (obj)
           (handler-case
               (progn (llm-protocol:stream-generate
                       (llm-protocol-openai:make-openai-compat-backend
                        :request-fn (%stream-error-fn obj))
                       "hi" :on-part (lambda (p) (declare (ignore p))))
                      nil)
             (llm-protocol:llm-error (c) c))))
    (let ((c (fail-with (%ht "error" (%ht "message" "upstream died")))))
      (ok (typep c 'llm-protocol:llm-http-error))
      (ok (eql 500 (llm-protocol:llm-http-error-status c)))
      (ok (llm-protocol:llm-http-error-retryable-p c)))
    (let ((c (fail-with (%ht "error" (%ht "message" "Server is overloaded" "code" 503)))))
      (ok (typep c 'llm-protocol:llm-overloaded)))
    (let ((c (fail-with (%ht "error" (%ht "message" "the request exceeds the available context size"
                                          "type" "exceed_context_size_error" "code" 400)))))
      (ok (typep c 'llm-protocol:llm-context-overflow)))))

(defun %models-fn (models &optional (counter (list 0)))
  (lambda (method url &key &allow-other-keys)
    (declare (ignore method))
    (cond ((search "/models" url)
           (incf (car counter))
           (values 200 (stack-json:encode (%ht "data" (coerce models 'vector)))))
          (t (values 404 "{}")))))

(deftest openai-context-window-from-models
  (let* ((counter (list 0))
         (b (llm-protocol-openai:make-openai-compat-backend
             :request-fn (%models-fn (list (%ht "id" "lm" "max_context_length" 32768)
                                           (%ht "id" "vl" "max_model_len" 131072)
                                           (%ht "id" "or" "context_length" 200000)
                                           (%ht "id" "none"))
                                     counter)
             :model-catalog nil)))
    (ok (eql 32768 (llm-protocol-openai:openai-model-context-window b "lm")))
    (ok (eql 131072 (llm-protocol-openai:openai-model-context-window b "vl")))
    (ok (eql 200000 (llm-protocol-openai:openai-model-context-window b "or")))
    (ok (null (llm-protocol-openai:openai-model-context-window b "none")))
    (ok (null (llm-protocol-openai:openai-model-context-window b "missing")))
    (ok (= 1 (car counter)) "the listing is fetched once")
    (ok (eql 32768 (llm-protocol:context-window b "lm")))))

(deftest openai-context-window-failure-is-nil
  (let ((b (llm-protocol-openai:make-openai-compat-backend
            :request-fn (%raise-fn 'http-protocol:http-connection-error "refused")
            :model-catalog nil)))
    (ok (null (llm-protocol-openai:openai-model-context-window b "x")))))

(deftest openai-overflow-markers
  (ok (llm-protocol-openai:openai-context-overflow-text-p "EXCEED_CONTEXT_SIZE_ERROR"))
  (ok (llm-protocol-openai:openai-context-overflow-text-p "context_length_exceeded"))
  (ng (llm-protocol-openai:openai-context-overflow-text-p "tools[0] invalid"))
  (ng (llm-protocol-openai:openai-context-overflow-text-p nil)))
