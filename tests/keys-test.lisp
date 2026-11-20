(in-package #:llm-protocol-openai/tests)

;;; API key resolution (llm-protocol#10) and redaction (http-backend-async#54).

(defparameter +key+ "sk-openai-redact-4b8e2f17c9")
(defparameter +env-key+ "sk-openai-from-env-91d3a0")

(defun %token-free-p (string)
  (not (or (search +key+ string) (search +env-key+ string))))

(defmacro %with-env ((&rest bindings) &body body)
  "Bind env vars around BODY. NIL means unset (\"\": %ENV treats it as absent)."
  (let ((saved (gensym)))
    `(let ((,saved (mapcar (lambda (n) (cons n (uiop:getenv n)))
                           (list ,@(mapcar #'first bindings)))))
       (unwind-protect
            (progn
              ,@(loop for (name value) in bindings
                      collect `(setf (uiop:getenv ,name) (or ,value "")))
              ,@body)
         (loop for (n . v) in ,saved
               do (setf (uiop:getenv n) (or v "")))))))

(defun %auth-header (backend)
  (cdr (assoc "authorization" (llm-protocol-openai::%headers backend)
              :test #'string-equal)))

(deftest openai-api-key-not-supplied-reads-env
  (%with-env (("OPENAI_API_KEY" +env-key+) ("LM_API_TOKEN" nil))
    (let ((b (llm-protocol-openai:make-openai-compat-backend)))
      (ok (equal +env-key+ (llm-protocol-openai:openai-api-key b)))
      (ok (equal (concatenate 'string "Bearer " +env-key+)
                 (http-protocol:reveal (%auth-header b)))))
    (ok (equal +env-key+ (llm-protocol-openai:openai-api-key
                          (llm-protocol-openai:make-openai-compat-backend
                           :api-key :env)))))
  (%with-env (("OPENAI_API_KEY" nil) ("LM_API_TOKEN" +env-key+))
    (ok (equal +env-key+ (llm-protocol-openai:openai-api-key
                          (llm-protocol-openai:make-openai-compat-backend))))))

(deftest openai-api-key-nil-or-none-means-no-key
  (%with-env (("OPENAI_API_KEY" +env-key+) ("LM_API_TOKEN" +env-key+))
    (dolist (none (list nil :none ""))
      (let ((b (llm-protocol-openai:make-openai-compat-backend :api-key none)))
        (ok (null (llm-protocol-openai:openai-api-key b)))
        (ok (null (%auth-header b)))))))

(deftest openai-api-key-explicit
  (%with-env (("OPENAI_API_KEY" +env-key+))
    (let ((b (llm-protocol-openai:make-openai-compat-backend :api-key +key+)))
      (ok (equal +key+ (llm-protocol-openai:openai-api-key b))))
    (let ((b (llm-protocol-openai:make-openai-compat-backend
              :api-key (http-protocol:make-secret +key+))))
      (ok (equal +key+ (llm-protocol-openai:openai-api-key b)))
      (setf (llm-protocol-openai:openai-api-key b) nil)
      (ok (null (%auth-header b)))))
  (ok (signals (llm-protocol-openai:make-openai-compat-backend :api-key 42)
               'llm-protocol:llm-error)))

(deftest openai-key-never-prints
  (let* ((bt nil)
         (seen nil)
         (b (llm-protocol-openai:make-openai-compat-backend
             :api-key +key+
             :base-url "http://example.invalid/v1"
             :request-fn
             (lambda (method url &key headers content want-stream)
               (declare (ignore method url content want-stream))
               (setf seen headers
                     bt (with-output-to-string (s)
                             (uiop:print-backtrace :stream s :count 60)))
               (values 401 "{\"error\":{\"message\":\"invalid api key\"}}"))))
         (err (handler-case (llm-protocol:generate b "hi" :model "local")
                (llm-protocol:llm-http-error (e) e))))
    ;; the key is on the header, as a SECRET
    (ok (http-protocol:secret-p (cdr (assoc "authorization" seen :test #'string-equal))))
    (ok (equal (concatenate 'string "Bearer " +key+)
               (http-protocol:reveal
                (cdr (assoc "authorization" seen :test #'string-equal)))))
    ;; ...but no printed form shows it
    (ok (typep err 'llm-protocol:llm-http-error))
    (ok (%token-free-p (format nil "~A ~S" err err)))
    (ok (%token-free-p (prin1-to-string seen)))
    (ok (%token-free-p (prin1-to-string b)))
    (ok (%token-free-p (with-output-to-string (s) (describe b s))))
    (ok (search "headers" (string-downcase bt)))
    (ok (%token-free-p bt))))
