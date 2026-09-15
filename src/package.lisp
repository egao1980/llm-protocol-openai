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
           #:openai-model-flags
           #:openai-model-flag
           #:lookup-model-flags
           #:model-catalog-flag
           #:copy-default-model-catalog
           #:+default-openai-base-url+
           #:+default-openai-embedding-model+
           #:+default-local-model-flags+
           #:encode-image-part))

(in-package #:llm-protocol-openai)
