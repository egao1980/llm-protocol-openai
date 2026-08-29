(defsystem "llm-protocol-openai"
  :version "0.2.1"
  :description "OpenAI chat/completions + Responses backend for llm-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ("llm-protocol" "http-protocol" "json-protocol" "json-backend-jzon"
               "sse-protocol" "babel")
  :properties
  (:cl-repo
   (:ci (:with ("capability-protocol"
                "http-backend-async"
                "event-backend-libuv"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "backend")
               (:file "stream"))
  :in-order-to ((test-op (test-op "llm-protocol-openai/tests"))))

(defsystem "llm-protocol-openai/tests"
  :depends-on ("llm-protocol-openai" "llm-protocol/capability"
               "http-backend-async" "event-backend-libuv"
               "usocket" "bordeaux-threads" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
