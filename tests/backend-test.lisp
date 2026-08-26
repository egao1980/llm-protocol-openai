(in-package #:llm-protocol-openai/tests)

(defun %ht (&rest kvs)
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on kvs by #'cddr
          unless (null v)
            do (setf (gethash k h) v))
    h))

(defun %fake-openai (method url &key headers content)
  (declare (ignore headers))
  (cond
    ((and (eq method :post) (search "/chat/completions" url))
     (let* ((body (stack-json:decode content))
            (msgs (gethash "messages" body))
            (last (elt msgs (1- (length msgs))))
            (tools (gethash "tools" body)))
       (values 200
               (stack-json:encode
                (%ht "model" (or (gethash "model" body) "gpt-4o-mini")
                     "usage" (%ht "prompt_tokens" 3 "completion_tokens" 2
                                  "total_tokens" 5)
                     "choices"
                     (vector (%ht "finish_reason" (if tools "tool_calls" "stop")
                                  "message"
                                  (%ht "role" "assistant"
                                       "content" (if tools
                                                     :null
                                                     (format nil "ok:~a"
                                                             (gethash "content" last)))
                                       "tool_calls"
                                       (when tools
                                         (vector (%ht "id" "call_1"
                                                      "type" "function"
                                                      "function"
                                                      (%ht "name" "sum"
                                                           "arguments" "{\"a\":1}"))))))))))))
    ((and (eq method :post) (search "/responses" url))
     (let* ((body (stack-json:decode content))
            (input (gethash "input" body))
            (tools (gethash "tools" body))
            (text (if (stringp input)
                      input
                      (let ((last (elt input (1- (length input)))))
                        (or (gethash "text" last)
                            (let ((c (gethash "content" last)))
                              (if (and c (plusp (length c)))
                                  (gethash "text" (elt c 0))
                                  "")))))))
       (values 200
               (stack-json:encode
                (%ht "id" "resp_1"
                     "status" "completed"
                     "model" (or (gethash "model" body) "gpt-4o-mini")
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
                                                   "text" (format nil "ok:~a" text)))))))))))
    ((search "/models" url)
     (values 200 (stack-json:encode
                  (%ht "data" (vector (%ht "id" "local" "owned_by" "lmstudio"))))))
    (t (values 404 "{}"))))

(defun %fake-openai-error (method url &key headers content)
  (declare (ignore method url headers content))
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
    (flet ((capture (method url &key headers content)
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
    (flet ((capture (method url &key headers content)
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

(deftest openai-stream-unsupported
  (ok (signals (llm-protocol:stream-generate
                (llm-protocol-openai:make-openai-compat-backend :request-fn #'%fake-openai)
                "hi")
               'llm-protocol:llm-unsupported)))

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
    (flet ((capture (method url &key headers content)
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
    (let ((gen (capability-protocol:get-capability cat :llm-generation)))
      (ng (find 'capability-protocol:stream-complete
                (capability-protocol:capability-operations gen)
                :key #'capability-protocol:capability-operation-name)))))

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
      (multiple-value-bind (status json)
          (%fake-openai method url :headers nil :content (or body "{}"))
        (let* ((octets (babel:string-to-octets json :encoding :utf-8))
               (head (babel:string-to-octets
                      (format nil
                              "HTTP/1.1 ~a OK~C~CContent-Type: application/json~C~CContent-Length: ~a~C~CConnection: close~C~C~C~C"
                              status #\Return #\Newline #\Return #\Newline
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
         (ok (equal "local" (llm-protocol:llm-model-info-id (first models)))))))))

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
