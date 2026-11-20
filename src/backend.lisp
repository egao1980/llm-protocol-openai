(in-package #:llm-protocol-openai)

(defparameter +default-openai-base-url+ "http://127.0.0.1:1234/v1"
  "LM Studio OpenAI-compatible default.")

(defparameter +default-openai-embedding-model+ "text-embedding-3-small")

(defun %env (name)
  (let ((v (uiop:getenv name)))
    (and v (plusp (length v)) v)))

(defclass openai-compat-backend (llm-backend)
  ((base-url :initarg :base-url :accessor openai-base-url
             :initform +default-openai-base-url+)
   (api-key :initarg :api-key :initform nil
            :documentation "NIL or an HTTP-PROTOCOL:SECRET (prints redacted).
OPENAI-API-KEY reads the plain string.")
   (default-model :initarg :default-model :accessor openai-default-model
                  :initform "gpt-4o-mini")
   (embedding-model :initarg :embedding-model :accessor openai-embedding-model
                    :initform +default-openai-embedding-model+)
   (organization :initarg :organization :accessor openai-organization :initform nil)
   (request-fn :initarg :request-fn :accessor openai-request-fn :initform nil)
   (model-catalog :initarg :model-catalog :accessor openai-model-catalog
                  :initform nil)
   (prompt-cache-key :initarg :prompt-cache-key :accessor openai-prompt-cache-key
                     :initform nil
                     :documentation "Default prompt_cache_key (string or NIL): the
routing hint for OpenAI's automatic prefix cache, sent on /chat/completions and
/responses. Per call: settings extra :prompt-cache-key (NIL or :omit drops it).")))

(defun %resolve-api-key (value supplied-p &rest env-names)
  "API key from a constructor argument (llm-protocol#10).
   Not supplied, or :ENV → the first non-empty ENV-NAMES variable.
   NIL or :NONE → no key: the environment is not read.
   A string or HTTP-PROTOCOL:SECRET → that key (an empty string is no key)."
  (cond ((or (not supplied-p) (eq value :env))
         (some #'%env env-names))
        ((or (null value) (eq value :none)) nil)
        ((http-protocol:secret-p value) value)
        ((stringp value) (and (plusp (length value)) value))
        (t (error 'llm-error
                  :message (format nil "api-key must be a string, NIL, :none or :env, got a ~(~A~)"
                                   (type-of value))))))

(defun %key-secret (value)
  "VALUE (string, SECRET or NIL) as a SECRET, or NIL."
  (let ((v (http-protocol:reveal value)))
    (and (stringp v) (plusp (length v)) (http-protocol:make-secret value))))

(defmethod initialize-instance :after ((b openai-compat-backend) &key)
  (setf (slot-value b 'api-key) (%key-secret (slot-value b 'api-key))))

(defgeneric openai-api-key (backend)
  (:documentation "The plain API key string, or NIL.")
  (:method ((b openai-compat-backend))
    (http-protocol:reveal (slot-value b 'api-key))))

(defgeneric (setf openai-api-key) (value backend)
  (:documentation "Set the key: a string, an HTTP-PROTOCOL:SECRET, or NIL.")
  (:method (value (b openai-compat-backend))
    (setf (slot-value b 'api-key) (%key-secret value))
    value))

(defun make-openai-compat-backend (&key base-url (api-key nil api-key-p) default-model
                                     embedding-model organization request-fn
                                     prompt-cache-key
                                     (model-catalog nil model-catalog-supplied-p))
  (check-type prompt-cache-key (or null string))
  (make-instance 'openai-compat-backend
                 :prompt-cache-key prompt-cache-key
                 :base-url (or base-url (%env "OPENAI_BASE_URL")
                               +default-openai-base-url+)
                 :api-key (%resolve-api-key api-key api-key-p "OPENAI_API_KEY" "LM_API_TOKEN")
                 :default-model (or default-model (%env "OPENAI_MODEL") "gpt-4o-mini")
                 :embedding-model (or embedding-model (%env "OPENAI_EMBEDDING_MODEL")
                                      +default-openai-embedding-model+)
                 :organization (or organization (%env "OPENAI_ORGANIZATION"))
                 :request-fn request-fn
                 :model-catalog (if model-catalog-supplied-p
                                    model-catalog
                                    (copy-default-model-catalog))))

(defun openai-model-flags (backend model)
  (lookup-model-flags (openai-model-catalog backend) model))

(defun openai-model-flag (backend model flag &optional default)
  (model-catalog-flag (openai-model-catalog backend) model flag default))

(defun use-openai-compat-backend (&rest args &key &allow-other-keys)
  (setf *llm-backend* (apply #'make-openai-compat-backend args)))

(defmethod backend-model ((backend openai-compat-backend))
  (openai-default-model backend))

(defmethod backend-supports-p ((backend openai-compat-backend) (feature (eql :tools)))
  t)

(defmethod backend-supports-p ((backend openai-compat-backend)
                               (feature (eql :structured-output)))
  t)

(defmethod backend-supports-p ((backend openai-compat-backend) (feature (eql :vision)))
  t)

(defmethod backend-supports-p ((backend openai-compat-backend) (feature (eql :responses)))
  t)

(defmethod backend-supports-p ((backend openai-compat-backend) (feature (eql :stream)))
  t)

(defmethod backend-supports-p ((backend openai-compat-backend) (feature (eql :embeddings)))
  t)

(defun %ht (&rest kvs)
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on kvs by #'cddr
          unless (or (null k) (eq v :omit) (null v))
            do (setf (gethash k h) v))
    h))

(defun %wire-key (key)
  (cond
    ((stringp key) key)
    ((or (keywordp key) (symbolp key))
     (substitute #\_ #\- (string-downcase (symbol-name key))))
    (t (princ-to-string key))))

(defun %wire-extra-value (value)
  (cond
    ((eq value :omit) :omit)
    ((keywordp value) (string-downcase (symbol-name value)))
    ((hash-table-p value) value)
    ((and (consp value) (keywordp (car value)))
     (let ((h (make-hash-table :test 'equal)))
       (loop for (k v) on value by #'cddr
             unless (or (null k) (eq v :omit))
               do (setf (gethash (%wire-key k) h) (%wire-extra-value v)))
       h))
    (t value)))

(defparameter +%extra-owned-keys+ '("prompt_cache_key")
  "Extra keys the backend resolves itself (backend default + per-call
override); %apply-settings-extra never copies them raw.")

(defun %apply-settings-extra (body settings)
  "Merge LLM-SETTINGS-EXTRA onto BODY. First-class keys already present win.
   NIL is kept (JSON false). Use :omit to skip a key."
  (let ((extra (and settings (llm-settings-extra settings))))
    (when extra
      (flet ((put (k v)
               (let ((key (%wire-key k))
                     (val (%wire-extra-value v)))
                 (unless (or (eq val :omit) (nth-value 1 (gethash key body))
                             (member key +%extra-owned-keys+ :test #'equal))
                   (setf (gethash key body) val)))))
        (cond
          ((hash-table-p extra)
           (maphash #'put extra))
          ((and (consp extra) (or (keywordp (car extra)) (stringp (car extra))))
           (loop for (k v) on extra by #'cddr
                 do (put k v)))
          (t (error 'llm-error
                    :message (format nil "llm-settings-extra not a plist or hash: ~s"
                                     extra)))))))
  body)

(defun %settings-extra-get (settings key)
  "→ (values value found-p) for KEY (a keyword) in SETTINGS extra.
Matches kebab keywords and snake wire strings alike."
  (let ((extra (and settings (llm-settings-extra settings)))
        (wire (%wire-key key)))
    (flet ((hit-p (k) (equal (%wire-key k) wire)))
      (cond
        ((hash-table-p extra)
         (block found
           (maphash (lambda (k v) (when (hit-p k) (return-from found (values v t))))
                    extra)
           (values nil nil)))
        ((consp extra)
         (loop for (k v) on extra by #'cddr
               when (hit-p k) return (values v t)
               finally (return (values nil nil))))
        (t (values nil nil))))))

(defun %prompt-cache-key (backend settings)
  "prompt_cache_key for this call: settings extra :prompt-cache-key, else the
backend default. NIL or :omit in extra means none."
  (multiple-value-bind (v found) (%settings-extra-get settings :prompt-cache-key)
    (let ((key (if found v (openai-prompt-cache-key backend))))
      (cond
        ((or (null key) (eq key :omit)) nil)
        ((stringp key) key)
        (t (error 'llm-error
                  :message (format nil "prompt-cache-key must be a string, got ~s"
                                   key)))))))

(defun %join (base path)
  (format nil "~a~a" (string-right-trim "/" (or base "")) path))

(defun %headers (backend &key (accept "application/json"))
  (let ((h `(("content-type" . "application/json")
             ("accept" . ,accept))))
    (let ((key (slot-value backend 'api-key)))
      (when key
        ;; a SECRET: backtraces and conditions print it redacted (#54)
        (push (cons "authorization"
                    (http-protocol:make-secret
                     (concatenate 'string "Bearer " (http-protocol:reveal key))))
              h)))
    (when (openai-organization backend)
      (push (cons "openai-organization" (openai-organization backend)) h))
    h))

(defun %body-string (response)
  (let ((b (http-protocol:response-body response)))
    (cond
      ((stringp b) b)
      ((and (vectorp b) (not (stringp b)))
       (babel:octets-to-string b :encoding :utf-8))
      (t ""))))

(defun %http-request (method url &key headers content want-stream)
  (unless http-protocol:*http-backend*
    (error 'llm-error
           :message "*http-backend* is nil — bind an http-protocol backend"))
  (let ((res (apply #'http:request method url
                    :headers headers
                    :timeout 180
                    :want-stream (and want-stream t)
                    (and content (list :content content)))))
    (values (http-protocol:response-status res)
            (if want-stream
                (http-protocol:response-body res)
                (%body-string res)))))

(defun %request (backend method path &key object want-stream accept)
  (let* ((fn (or (openai-request-fn backend) #'%http-request))
         (url (%join (openai-base-url backend) path))
         (content (and object (stack-json:encode object)))
         (headers (%headers backend
                            :accept (or accept
                                        (if want-stream
                                            "text/event-stream"
                                            "application/json")))))
    (multiple-value-bind (status body)
        (funcall fn method url :headers headers :content content
                 :want-stream want-stream)
      (values status body))))

(defun %error-message (obj fallback)
  (cond
    ((and (hash-table-p obj) (hash-table-p (gethash "error" obj)))
     (or (gethash "message" (gethash "error" obj)) fallback))
    ((and (hash-table-p obj) (gethash "error" obj))
     (let ((err (gethash "error" obj)))
       (if (stringp err) err (princ-to-string err))))
    (t fallback)))

(defun %decode (status body)
  (let ((obj (ignore-errors (stack-json:decode body))))
    (cond
      ((<= 200 status 299) (or obj (error 'llm-error :message "empty JSON body")))
      (t
       (restart-case
           (error 'llm-http-error
                  :status status
                  :body body
                  :retryable-p (http-status-retryable-p status)
                  :message (%error-message obj (format nil "HTTP ~a" status)))
         (retry ()
           :report "Retry the HTTP request"
           (llm-protocol::%invoke-retry))
         (use-value (value)
           :report "Use a supplied decoded object"
           value))))))

(defun %str (x)
  (cond
    ((or (null x) (eq x :null)) "")
    ((stringp x) x)
    (t (princ-to-string x))))

(defun %blank-text-p (x)
  (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) (%str x)))))

(defun %effective-max-tokens (backend model settings)
  "Apply catalog :min-completion-tokens as a floor on SETTINGS max-tokens."
  (let* ((requested (and settings (llm-settings-max-tokens settings)))
         (floor (openai-model-flag backend model :min-completion-tokens)))
    (if (and (integerp floor) (plusp floor) (integerp requested))
        (max requested floor)
        requested)))

(defun %maybe-reasoning-as-content (backend model content thinking)
  "When :reasoning-as-content is on and chat content is blank, use reasoning."
  (if (and backend
           (openai-model-flag backend model :reasoning-as-content)
           (%blank-text-p content)
           (not (%blank-text-p thinking)))
      (%str thinking)
      content))

(defparameter +%b64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun %usb8-p (x)
  (and (vectorp x)
       (not (stringp x))
       (or (zerop (length x))
           (integerp (aref x 0)))))

(defun %rfc4648-encode (octets)
  (let* ((octets (coerce octets '(simple-array (unsigned-byte 8) (*))))
         (n (length octets))
         (out (make-string (* 4 (ceiling n 3)) :initial-element #\=)))
    (loop with j = 0
          for i from 0 below n by 3
          for b0 = (aref octets i)
          for b1 = (if (< (1+ i) n) (aref octets (1+ i)) 0)
          for b2 = (if (< (+ i 2) n) (aref octets (+ i 2)) 0)
          for triple = (logior (ash b0 16) (ash b1 8) b2)
          do (setf (char out j) (char +%b64-alphabet+ (ldb (byte 6 18) triple))
                   (char out (1+ j)) (char +%b64-alphabet+ (ldb (byte 6 12) triple)))
             (when (< (1+ i) n)
               (setf (char out (+ j 2))
                     (char +%b64-alphabet+ (ldb (byte 6 6) triple))))
             (when (< (+ i 2) n)
               (setf (char out (+ j 3))
                     (char +%b64-alphabet+ (ldb (byte 6 0) triple))))
             (incf j 4))
    out))

(defun %image-data-base64 (part)
  (let ((data (llm-image-part-data part)))
    (cond
      ((null data) nil)
      ((and (stringp data) (eql 0 (search "data:" data))) data)
      ((stringp data) data)
      ((%usb8-p data) (%rfc4648-encode data))
      (t (princ-to-string data)))))

(defun %image-data-url (part)
  (let ((data (%image-data-base64 part)))
    (cond
      ((null data) nil)
      ((and (stringp data) (eql 0 (search "data:" data))) data)
      (t (format nil "data:~a;base64,~a"
                 (or (llm-image-part-media-type part) "image/png")
                 data)))))

(defun encode-image-part (part &key (style :chat))
  "Encode LLM-IMAGE-PART for the OpenAI wire (url or base64 data + media-type).
   :chat → {type:image_url, image_url:{url}}
   :responses → {type:input_image, image_url}"
  (check-type part llm-image-part)
  (let ((url (or (llm-image-part-url part) (%image-data-url part))))
    (ecase style
      (:chat
       (%ht "type" "image_url"
            "image_url" (%ht "url" url)))
      (:responses
       (%ht "type" "input_image"
            "image_url" url)))))

(defgeneric %wire-part (part)
  (:method ((part llm-text-part))
    (%ht "type" "text" "text" (or (llm-text-part-text part) "")))
  (:method ((part llm-image-part))
    (encode-image-part part :style :chat))
  (:method ((part llm-thinking-part))
    nil)
  (:method ((part llm-part))
    nil))

(defun %wire-tool-call (part)
  (%ht "id" (or (llm-tool-call-part-id part) "call_0")
       "type" "function"
       "function" (%ht "name" (llm-tool-call-part-name part)
                       "arguments"
                       (let ((a (llm-tool-call-part-arguments part)))
                         (if (stringp a) a (stack-json:encode a))))))

(defun %wire-turn (turn)
  (let* ((turn (coerce-turn turn))
         (role (string-downcase (symbol-name (llm-turn-role turn))))
         (texts (remove nil (mapcar #'%wire-part (llm-turn-parts turn))))
         (calls (remove-if-not #'llm-tool-call-part-p (llm-turn-parts turn)))
         (results (remove-if-not #'llm-tool-result-part-p (llm-turn-parts turn)))
         (thinking (find-if #'llm-thinking-part-p (llm-turn-parts turn))))
    (cond
      ((eq (llm-turn-role turn) :tool)
       (let ((r (or (first results)
                    (make-llm-tool-result-part :id nil :content (turn-text turn)))))
         (%ht "role" "tool"
              "tool_call_id" (llm-tool-result-part-id r)
              "name" (llm-tool-result-part-name r)
              "content" (or (llm-tool-result-part-content r) ""))))
      (t
       (let ((content (cond
                        ((and texts (null (rest texts))
                              (equal (gethash "type" (first texts)) "text")
                              (null calls))
                         (gethash "text" (first texts)))
                        (texts (map 'vector #'identity texts))
                        (t ""))))
         (let ((h (%ht "role" role "content" content)))
           (when calls
             (setf (gethash "tool_calls" h)
                   (map 'vector #'%wire-tool-call calls)))
           (when thinking
             (setf (gethash "reasoning_content" h) (llm-thinking-part-text thinking)))
           h))))))

(defun %wire-tool (tool)
  (cond
    ((llm-tool-p tool)
     (%ht "type" "function"
          "function" (%ht "name" (llm-tool-name tool)
                          "description" (llm-tool-description tool)
                          "parameters" (or (llm-tool-parameters tool)
                                           (%ht "type" "object"
                                                "properties" (%ht))))))
    ((hash-table-p tool) tool)
    ((and (consp tool) (keywordp (car tool)))
     (%wire-tool (make-llm-tool :name (getf tool :name)
                                :description (getf tool :description)
                                :parameters (getf tool :parameters))))
    (t (error 'llm-error :message (format nil "not a tool: ~s" tool)))))

(defun %wire-responses-tool (tool)
  (cond
    ((llm-tool-p tool)
     (%ht "type" "function"
          "name" (llm-tool-name tool)
          "description" (llm-tool-description tool)
          "parameters" (or (llm-tool-parameters tool)
                           (%ht "type" "object" "properties" (%ht)))))
    ((hash-table-p tool) tool)
    ((and (consp tool) (keywordp (car tool)))
     (%wire-responses-tool (make-llm-tool :name (getf tool :name)
                                          :description (getf tool :description)
                                          :parameters (getf tool :parameters))))
    (t (error 'llm-error :message (format nil "not a tool: ~s" tool)))))

(defun %wire-tool-choice (choice)
  (etypecase choice
    (null nil)
    ((eql :auto) "auto")
    ((eql :none) "none")
    ((eql :required) "required")
    (string (%ht "type" "function" "function" (%ht "name" choice)))
    (hash-table choice)))

(defun %wire-item-part (part inputp)
  (etypecase part
    (llm-text-part
     (%ht "type" (if inputp "input_text" "output_text")
          "text" (or (llm-text-part-text part) "")))
    (llm-image-part
     (encode-image-part part :style :responses))
    (llm-part nil)))

(defun %wire-item (item)
  (etypecase item
    (llm-message-item
     (let* ((role (llm-message-item-role item))
            (inputp (not (eq role :assistant)))
            (parts (remove nil (mapcar (lambda (p) (%wire-item-part p inputp))
                                       (llm-message-item-parts item)))))
       (%ht "type" "message"
            "role" (string-downcase (symbol-name role))
            "content" (map 'vector #'identity parts))))
    (llm-function-call-item
     (%ht "type" "function_call"
          "call_id" (or (llm-function-call-item-call-id item) (llm-item-id item))
          "name" (llm-function-call-item-name item)
          "arguments" (let ((a (llm-function-call-item-arguments item)))
                        (if (stringp a) a (stack-json:encode a)))))
    (llm-function-call-output-item
     (%ht "type" "function_call_output"
          "call_id" (llm-function-call-output-item-call-id item)
          "output" (or (llm-function-call-output-item-output item) "")))
    (llm-reasoning-item
     (%ht "type" "reasoning"
          "summary" (vector (%ht "type" "summary_text"
                                 "text" (or (llm-reasoning-item-text item) "")))))))

(defun %scalar-user-input (items)
  (when (and (null (rest items))
             (llm-message-item-p (first items))
             (eq :user (llm-message-item-role (first items)))
             (= 1 (length (llm-message-item-parts (first items))))
             (llm-text-part-p (first (llm-message-item-parts (first items)))))
    (llm-text-part-text (first (llm-message-item-parts (first items))))))

(defun %finish-reason (raw)
  (cond
    ((or (null raw) (eq raw :null)) :stop)
    ((string-equal raw "stop") :stop)
    ((string-equal raw "length") :length)
    ((or (string-equal raw "tool_calls") (string-equal raw "tool_use")) :tool-use)
    ((string-equal raw "content_filter") :content-filter)
    (t :stop)))

(defun %cached-tokens (obj)
  "cached_tokens from prompt_tokens_details (chat) or input_tokens_details
(responses), or NIL when the provider did not report it."
  (loop for key in '("prompt_tokens_details" "input_tokens_details")
        for details = (gethash key obj)
        for n = (and (hash-table-p details) (gethash "cached_tokens" details))
        when (integerp n) return n))

(defun %usage (obj)
  "OpenAI counts cached tokens inside prompt_tokens / input_tokens; llm-usage
keeps INPUT-TOKENS uncached and puts the cached share in CACHE-READ-TOKENS, so
LLM-USAGE-PROMPT-TOKENS is the reported prompt size. OpenAI reports no cache
writes. TOTAL-TOKENS is passed through."
  (when (hash-table-p obj)
    (let ((prompt (or (gethash "prompt_tokens" obj) (gethash "input_tokens" obj)))
          (cached (%cached-tokens obj)))
      (make-llm-usage
       :input-tokens (if (and (integerp prompt) cached)
                         (max 0 (- prompt cached))
                         prompt)
       :output-tokens (or (gethash "completion_tokens" obj) (gethash "output_tokens" obj))
       :total-tokens (gethash "total_tokens" obj)
       :cache-read-tokens cached))))

(defun %parse-response (obj requested-model &optional backend)
  (let* ((choice (let ((cs (gethash "choices" obj)))
                   (and cs (plusp (length cs)) (elt cs 0))))
         (msg (and choice (gethash "message" choice)))
         (raw-content (and msg (gethash "content" msg)))
         (tcs (and msg (gethash "tool_calls" msg)))
         (thinking (and msg (or (gethash "reasoning_content" msg)
                                (gethash "thinking" msg))))
         (content (%maybe-reasoning-as-content backend requested-model
                                               raw-content thinking))
         (parts (append
                 (and thinking (not (eq thinking :null))
                      (list (make-llm-thinking-part :text (%str thinking))))
                 (and content (not (eq content :null)) (plusp (length (%str content)))
                      (list (make-llm-text-part :text (%str content))))
                 (mapcar #'llm-protocol::%coerce-tool-call-part
                         (llm-protocol::%as-list tcs)))))
    (make-llm-response
     :parts parts
     :model (or (gethash "model" obj) requested-model)
     :finish-reason (%finish-reason (and choice (gethash "finish_reason" choice)))
     :usage (%usage (and obj (gethash "usage" obj))))))

(defun %parts-from-items (items)
  (loop for it in items
        append (etypecase it
                 (llm-message-item (copy-list (llm-message-item-parts it)))
                 (llm-function-call-item
                  (list (make-llm-tool-call-part
                         :id (or (llm-function-call-item-call-id it) (llm-item-id it))
                         :name (llm-function-call-item-name it)
                         :arguments (or (llm-function-call-item-arguments it) "{}"))))
                 (llm-reasoning-item
                  (list (make-llm-thinking-part
                         :text (or (llm-reasoning-item-text it) "")
                         :signature (llm-reasoning-item-signature it))))
                 (llm-function-call-output-item nil))))

(defun %responses-finish (obj items)
  (let* ((status (and (hash-table-p obj) (gethash "status" obj)))
         (details (and (hash-table-p obj) (gethash "incomplete_details" obj)))
         (reason (and (hash-table-p details) (gethash "reason" details))))
    (cond
      ((find-if #'llm-function-call-item-p items) :tool-use)
      ((and (stringp status) (string-equal status "incomplete")
            (string-equal reason "max_output_tokens"))
       :length)
      ((and (stringp status) (string-equal status "incomplete")
            (string-equal reason "content_filter"))
       :content-filter)
      (t :stop))))

(defun %parse-responses (obj requested-model)
  (let* ((raw (or (and (hash-table-p obj) (gethash "output" obj)) #()))
         (items (mapcar #'coerce-item (llm-protocol::%as-list raw))))
    (make-llm-response
     :id (and (hash-table-p obj) (gethash "id" obj))
     :items items
     :parts (%parts-from-items items)
     :model (or (and (hash-table-p obj) (gethash "model" obj)) requested-model)
     :finish-reason (%responses-finish obj items)
     :usage (%usage (and (hash-table-p obj) (gethash "usage" obj))))))

(defun %schema-name (schema)
  (cond
    ((symbolp schema) (string-downcase (symbol-name schema)))
    ((and (hash-table-p schema) (gethash "title" schema))
     (princ-to-string (gethash "title" schema)))
    (t "output")))

(defun %wire-chat-response-format (settings)
  (or (and settings (llm-settings-response-format settings))
      (let ((out (and settings (llm-settings-output settings))))
        (when out
          (%ht "type" "json_schema"
               "json_schema"
               (%ht "name" (%schema-name out)
                    "strict" t
                    "schema" (structured-output-json-schema out)))))))

(defun %wire-responses-text (settings)
  (let ((raw (and settings (llm-settings-response-format settings)))
        (out (and settings (llm-settings-output settings))))
    (cond
      (raw (%ht "format" raw))
      (out (%ht "format"
                (%ht "type" "json_schema"
                     "name" (%schema-name out)
                     "strict" t
                     "schema" (structured-output-json-schema out)))))))

(defun %chat-completion-body (backend turns &key model settings tools tool-choice
                              stream)
  (let* ((settings (coerce-settings settings))
         (model (or model (openai-default-model backend)))
         (body (%ht "model" model
                    "messages" (map 'vector #'%wire-turn (coerce-turns turns))
                    "temperature" (and settings (llm-settings-temperature settings))
                    "max_tokens" (%effective-max-tokens backend model settings)
                    "stop" (and settings (llm-settings-stop settings))
                    "top_p" (and settings (llm-settings-top-p settings))
                    "response_format" (%wire-chat-response-format settings)
                    "tools" (and tools (map 'vector #'%wire-tool
                                            (llm-protocol::%as-list tools)))
                    "tool_choice" (%wire-tool-choice tool-choice)
                    "prompt_cache_key" (%prompt-cache-key backend settings)
                    "stream" (if stream t :omit)
                    "stream_options" (if stream (%ht "include_usage" t) :omit))))
    (%apply-settings-extra body settings)
    (values model body)))

(defun %responses-body (backend items &key model settings tools tool-choice stream)
  (let* ((settings (coerce-settings settings))
         (model (or model (openai-default-model backend)))
         (normalized (coerce-items items))
         (input (or (%scalar-user-input normalized)
                    (map 'vector #'%wire-item normalized)))
         (body (%ht "model" model
                    "input" input
                    "temperature" (and settings (llm-settings-temperature settings))
                    "max_output_tokens" (%effective-max-tokens backend model settings)
                    "top_p" (and settings (llm-settings-top-p settings))
                    "text" (%wire-responses-text settings)
                    "tools" (and tools (map 'vector #'%wire-responses-tool
                                            (llm-protocol::%as-list tools)))
                    "tool_choice" (%wire-tool-choice tool-choice)
                    "prompt_cache_key" (%prompt-cache-key backend settings)
                    "stream" (if stream t :omit))))
    (%apply-settings-extra body settings)
    (values model body)))

(defmethod generate ((backend openai-compat-backend) turns &key model settings
                     tools tool-choice output)
  (declare (ignore output))
  (multiple-value-bind (model body)
      (%chat-completion-body backend turns :model model :settings settings
                             :tools tools :tool-choice tool-choice)
    (multiple-value-bind (status text)
        (%request backend :post "/chat/completions" :object body)
      (%parse-response (%decode status text) model backend))))

(defmethod respond ((backend openai-compat-backend) items &key model settings
                    tools tool-choice output)
  (declare (ignore output))
  (multiple-value-bind (model body)
      (%responses-body backend items :model model :settings settings
                       :tools tools :tool-choice tool-choice)
    (multiple-value-bind (status text)
        (%request backend :post "/responses" :object body)
      (%parse-responses (%decode status text) model))))

(defmethod list-models ((backend openai-compat-backend) &key)
  (multiple-value-bind (status text)
      (%request backend :get "/models")
    (let* ((obj (%decode status text))
           (data (or (and (hash-table-p obj) (gethash "data" obj)) #())))
      (mapcar (lambda (m)
                (make-llm-model-info
                 :id (if (hash-table-p m) (gethash "id" m) (princ-to-string m))
                 :owned-by (and (hash-table-p m) (gethash "owned_by" m))))
              (llm-protocol::%as-list data)))))

(defun %float-vec (seq)
  (map 'vector (lambda (x) (float x 1f0)) (llm-protocol::%as-list seq)))

(defun %parse-embeddings (obj requested-model)
  (let* ((raw (and (hash-table-p obj) (gethash "data" obj)))
         (rows (sort (copy-list (llm-protocol::%as-list raw)) #'<
                     :key (lambda (row)
                            (or (and (hash-table-p row) (gethash "index" row)) 0))))
         (embs (loop for row in rows
                     for i from 0
                     for vec = (and (hash-table-p row) (gethash "embedding" row))
                     do (when (stringp vec)
                          (error 'llm-unsupported
                                 :message "base64 embeddings are not supported"))
                     collect (make-llm-embedding
                              :vector (%float-vec vec)
                              :index (or (and (hash-table-p row) (gethash "index" row))
                                         i)))))
    (make-llm-embed-result
     :embeddings embs
     :model (or (and (hash-table-p obj) (gethash "model" obj)) requested-model)
     :usage (%usage (and (hash-table-p obj) (gethash "usage" obj))))))

(defmethod embed ((backend openai-compat-backend) inputs &key model dimensions
                  encoding-format)
  (when (and encoding-format
             (not (member encoding-format '(:float "float") :test #'equal)))
    (error 'llm-unsupported
           :message (format nil "wave-1 embeddings are float-only, got ~s"
                            encoding-format)))
  (let* ((texts (coerce-embed-inputs inputs))
         (model (or model (openai-embedding-model backend)))
         (body (%ht "model" model
                    "input" (if (null (rest texts)) (first texts)
                                (map 'vector #'identity texts))
                    "dimensions" (or dimensions :omit)
                    "encoding_format" "float")))
    (multiple-value-bind (status text)
        (%request backend :post "/embeddings" :object body)
      (%parse-embeddings (%decode status text) model))))
