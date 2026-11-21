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

**API key** (`:api-key`): left out, or `:env`, reads `OPENAI_API_KEY` then `LM_API_TOKEN`. `nil`, `:none` or `""` means no key: no `Authorization` header, and the environment is not read. A string or an `http-protocol:secret` is used as given. The backend keeps the key as a `secret`, and the `Authorization` header value is a `secret` too, so backtraces, condition reports and `describe` print `#<SECRET [redacted]>`. `openai-api-key` returns the plain string. A custom `:request-fn` gets header values that may be secrets: call `http-protocol:reveal` on them.

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

**Prompt cache.** OpenAI caches long prompt prefixes automatically; `prompt_cache_key` is the routing hint that keeps related requests (one agent run, one task) on the same cache. Set a backend default with `:prompt-cache-key "run-42"`, override it per call with settings `:extra (:prompt-cache-key "task-7")`, or drop it with `:extra (:prompt-cache-key nil)` (never sent as JSON false). It goes on `/chat/completions` and `/responses`, streamed or not. Other cache fields pass through extra unchanged, e.g. `:extra (:prompt-cache-retention "24h")`. Usage maps `prompt_tokens_details.cached_tokens` (chat) and `input_tokens_details.cached_tokens` (Responses, also the streamed final usage chunk and `response.completed`) to `llm-usage-cache-read-tokens`. OpenAI counts cached tokens inside the prompt total; `llm-usage-input-tokens` is the uncached share, so `llm-usage-prompt-tokens` gives the full prompt as on Anthropic. OpenAI reports no cache writes (`cache-write-tokens` stays `nil`).

```lisp
(let ((b (stack-llm-openai:make-openai-compat-backend :prompt-cache-key "run-42")))
  (stack-llm:llm-usage-cache-read-tokens
   (stack-llm:llm-response-usage (stack-llm:generate b "ping"))))
```

`stream-generate` → `POST /chat/completions` with `stream: true` (chat.completion.chunk SSE, `[DONE]`). `stream-respond` → `POST /responses` with `stream: true` (typed events: `response.output_text.delta`, `response.reasoning_text.delta` / `response.reasoning_summary_text.delta`, `response.function_call_arguments.*`, `response.completed` / `response.failed`). Compat servers that stream `/responses` as chat chunks are accepted. `:on-part` gets text/thinking deltas; assembled `llm-response` is returned at EOF.

**Truncated streams.** The stream must end on a terminal signal. Chat: a `finish_reason` in some chunk (with or without `[DONE]`: success with that reason) or `[DONE]` alone (success, `:stop`, or `:tool-use` when calls arrived). Responses: `response.completed` / `response.incomplete` (or `[DONE]`, or for chat-style chunks the chat rule). A body that closes without one, empty or cut mid-text or mid-tool-call, signals `llm-protocol:llm-incomplete-response` (an `llm-http-error`, retryable, so `with-auto-retry` and `call-with-retry-policy` re-run the call). `llm-incomplete-response-partial` holds what arrived, finish reason `:incomplete`; restart `use-partial-response` (`invoke-use-partial-response`) returns it, `use-value` substitutes another response. Half-built tool calls are kept in the partial response but never sent to `:on-part`. The non-streaming path is unchanged.

Tests: mock `request-fn` + a usocket fixture through async×libuv. Live LM Studio: `LLM_OPENAI_LIVE=1`. CI needs published `llm-protocol` on GHCR.

Part of [cl-stack](https://github.com/egao1980/cl-stack) ([#195](https://github.com/egao1980/cl-stack/issues/195)).

## License

MIT — see [LICENSE](LICENSE).
