# llm-protocol-openai

OpenAI-compatible HTTP backend for [`llm-protocol`](https://github.com/egao1980/llm-protocol): `POST /chat/completions` (`generate`), `POST /responses` (`respond`), `POST /embeddings` (`embed`). Not the protocol.

Transport is `http-protocol` — bind [`http-backend-async`](https://github.com/egao1980/http-backend-async) × [`event-backend-libuv`](https://github.com/egao1980/event-backend-libuv). Dexador is maintenance; do not default to it.

```lisp
(asdf:load-system "llm-protocol-openai")
(asdf:load-system "event-backend-libuv")
(asdf:load-system "http-backend-async")

(setf http-backend-async:*event-backend-maker*
      #'event-backend-libuv:make-libuv-backend)
(setf http-protocol:*http-backend*
      (http-backend-async:make-async-backend))

(let ((b (stack-llm-openai:make-openai-compat-backend)))
  (stack-llm:llm-response-text
   (stack-llm:generate b "ping" :settings '(:temperature 0 :max-tokens 32)))
  (stack-llm:llm-response-text
   (stack-llm:respond b "ping" :settings '(:temperature 0 :max-tokens 32)))
  (stack-llm:stream-generate b "ping" :on-part #'print)
  (stack-llm:stream-respond b "ping" :on-part #'print)
  (stack-llm:embed-query b "ping"))
```

`OPENAI_API_KEY` / `LM_API_TOKEN` / `OPENAI_BASE_URL` / `OPENAI_MODEL` / `OPENAI_EMBEDDING_MODEL` fill omitted initargs. Default chat model is `gpt-4o-mini`; embeddings default to `text-embedding-3-small`. Default base is LM Studio `http://127.0.0.1:1234/v1`. Wave-1 embeddings are float only.

Per-model flags live on the backend catalog (`openai-model-catalog`), not on `llm-model-info`. Built-ins: `nemotron-3-nano-4b` (`:min-completion-tokens` 128) and `zai-org/glm-4.6v-flash` (same floor plus `:reasoning-as-content`). `:min-completion-tokens` floors `max_tokens` / `max_output_tokens` so probe calls with a tiny budget do not empty `content` (`finish_reason: length`). `:reasoning-as-content` copies `reasoning_content` into assistant text when chat `content` is empty or whitespace. Pass `:model-catalog` to replace the built-ins; omit it to keep them.

```lisp
(stack-llm-openai:make-openai-compat-backend
  :model-catalog '(("local" :min-completion-tokens 128 :reasoning-as-content t)))
```

`llm-settings-extra` (plist or hash) is merged onto the JSON body after first-class fields. Kebab keywords → snake keys; keyword values → lowercase strings; nested plists → objects; `nil` → JSON false. First-class keys already on the body win. Responses escape hatch:

```lisp
(stack-llm:respond b "ping"
  :settings '(:extra (:previous-response-id "resp_123"
                      :instructions "be brief"
                      :reasoning (:effort :medium)
                      :store nil)))
```

`stream-generate` → `POST /chat/completions` with `stream: true` (chat.completion.chunk SSE, `[DONE]`). `stream-respond` → `POST /responses` with `stream: true` (typed events: `response.output_text.delta`, `response.reasoning_text.delta` / `response.reasoning_summary_text.delta`, `response.function_call_arguments.*`, `response.completed` / `response.failed`). Compat servers that stream `/responses` as chat chunks are accepted. `:on-part` gets text/thinking deltas; assembled `llm-response` is returned at EOF.

Tests: mock `request-fn` + a usocket fixture through async×libuv. Live LM Studio: `LLM_OPENAI_LIVE=1`. CI needs published `llm-protocol` on GHCR.

Part of [cl-stack](https://github.com/egao1980/cl-stack) ([#195](https://github.com/egao1980/cl-stack/issues/195)).

## License

MIT — see [LICENSE](LICENSE).
