(defpackage #:llm-protocol-openai
  (:use #:cl #:llm-protocol)
  (:nicknames #:stack-llm-openai)
  (:export #:openai-compat-backend
           #:make-openai-compat-backend
           #:use-openai-compat-backend
           #:openai-base-url
           #:openai-api-key
           #:openai-default-model
           #:+default-openai-base-url+))

(in-package #:llm-protocol-openai)
