(defpackage #:llm-protocol-openai
  (:use #:cl #:llm-protocol)
  (:nicknames #:stack-llm-openai)
  (:export #:openai-compat-backend
           #:make-openai-compat-backend
           #:use-openai-compat-backend
           #:openai-base-url
           #:openai-api-key
           #:openai-default-model
           #:openai-embedding-model
           #:openai-model-catalog
           #:openai-prompt-cache-key
           #:openai-request-timeout
           #:openai-stream-idle-timeout
           #:openai-connect-timeout
           #:+default-openai-request-timeout+
           #:+default-openai-stream-idle-timeout+
           #:+default-openai-connect-timeout+
           #:openai-model-flags
           #:openai-model-flag
           #:lookup-model-flags
           #:model-catalog-flag
           #:copy-default-model-catalog
           #:+default-openai-base-url+
           #:+default-openai-embedding-model+
           #:+default-local-model-flags+
           #:encode-image-part
           ;; 0.7.0 (src/errors.lisp)
           #:+openai-context-overflow-markers+
           #:openai-context-overflow-text-p
           #:openai-model-context-window))

(in-package #:llm-protocol-openai)
