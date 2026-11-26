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

**Timeouts** (seconds; `nil`, `0` or env `none` = no limit). Left out, each reads its env var, then the default:

| Initarg | Env | Default | Bounds |
|---|---|---|---|
| `:request-timeout` | `OPENAI_TIMEOUT` | 600 | a non-streaming request, in all (the server says nothing until the whole answer is ready) |
| `:stream-idle-timeout` | `OPENAI_STREAM_IDLE_TIMEOUT` | 300 | silence on a streaming response. Streams have no total cap: one that keeps producing tokens runs to the end |
| `:connect-timeout` | `OPENAI_CONNECT_TIMEOUT` | 30 | TCP + TLS setup |

They map onto `http-protocol` phases: `(:connect C :read IDLE)` for streams and `(:connect C :total T)` otherwise. The read (idle) phase needs `http-backend-async` 0.2.25 or later. A timeout signals an `llm-protocol:llm-transport-error` of kind `:timeout` that is still an `http-protocol:http-timeout-error` ("llm transport error (timeout): timed out talking to <base URL>: read timed out: no data for N s …"; see [Errors](#errors-080)). `http-retry` does not retry it (model calls are POSTs, and it only retries idempotent methods); llm-protocol's `retry-policy` retries it as `:transient` at the agent step. A custom `:request-fn` owns its own timeouts. Readers: `openai-request-timeout`, `openai-stream-idle-timeout`, `openai-connect-timeout`.

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

## Errors (0.8.0)

Every failure is an [llm-protocol](https://github.com/egao1980/llm-protocol) 0.7.0 type, so callers bind on types and never parse a server's error text. The rules are in the header of `src/errors.lisp`.

| Answer | Condition |
|---|---|
| 429 | `llm-rate-limited` (Retry-After honoured) |
| 503, 529, or a 5xx whose message or body says "overloaded" | `llm-overloaded` |
| 401, 403 | `llm-auth-error` |
| 404 | `llm-not-found` |
| 400 or 413 with overflow wording | `llm-context-overflow` |
| any other status | a plain `llm-http-error`, retryable by its status (500 yes, 400 no) |
| a transport failure | `llm-transport-error` (`:connect`, `:reset`, `:tls`, `:timeout`, `:canceled`) |

- **HTTP errors** go through `llm-protocol:make-llm-http-error`: provider `"openai"`, `llm-error-url` the base URL, `llm-error-model` the request's model (the embedding model for `embed`, `nil` for `list-models`), the server's `error.message` (else "HTTP <status>") and the raw body. The API key is never in a slot or a report, and the exact key (8 characters or more) is scrubbed from the server's message and body. Restarts, as in 0.6: `retry` (`llm-protocol-openai::retry`, re-runs the call through the outer retry) and `use-value` (a decoded object in place of the body).
- **Overflow wording**: llm-protocol's generic phrases plus the servers' own, `+openai-context-overflow-markers+` (OpenAI `context_length_exceeded`, llama.cpp `exceed_context_size_error` / "exceeds the available context size", LM Studio "context the overflows", vLLM "maximum context length", …), looked for in the message, `error.code`, `error.type` and the body. `openai-context-overflow-text-p` is the test.
- **Retry-After**: the built-in transport reads the header; a custom `:request-fn` may return it as a third value (a string or seconds; two values mean none). `llm-http-error-retry-after` gives seconds.
- **Transport**: a condition for which `llm-protocol:transport-error-kind-for` gives a kind, raised inside a request or while a stream is read, becomes an `llm-transport-error` with `:cause` the original, the kind, provider `"openai"`, the base URL and the message "<kind text> talking to <base URL>: <cause>". `llm-retry-class` is `:transient` (`nil` for `:canceled`). The condition is still every `http-protocol` type its cause was (a timeout is still an `http-timeout-error`, a cut body still an `http-incomplete-response` with its octet count), so handlers written for http-protocol types keep matching. Nothing else is caught: a bug in a `:request-fn` or a redirect loop passes through as it was, and so does any condition the caller's `:on-part` signals.
- **Stream errors**: a chat `{"error": …}` chunk, a Responses `error` or `response.failed` event, is typed by the same mapping. The status is the event's `status`, else the error's `code`, when it is an HTTP status (an integer from 100 to 599), else 500, so a mid-stream server failure is retryable. An event with no status whose text is overflow wording is `llm-context-overflow`, since an overflow never succeeds on retry.
- **Context windows**: `(openai-model-context-window backend "model-id")` gives an integer or `nil`. It reads `GET <base>/models` once per backend (cached, also when the request fails; setting `openai-base-url` drops the cache): the entry whose `id` is the model, its first positive integer of `max_context_length`, `loaded_context_length`, `context_length`, `max_model_len` (LM Studio, OpenRouter, vLLM). Any failure gives `nil`, never a condition. `llm-protocol:context-window` on this backend tries an `llm-model-info`'s own window, then `*llm-catalog*` (the model's window, then the provider's), then `openai-model-context-window`. It never calls `list-models`, so all lookups together cost at most one request per backend. `list-models` stays a fresh `GET /models`; its `llm-model-info`s carry the window, and it refreshes the cache.

Tests: mock `request-fn` + a usocket fixture through async×libuv. Live LM Studio: `LLM_OPENAI_LIVE=1`. CI needs published `llm-protocol` on GHCR.

Part of [cl-stack](https://github.com/egao1980/cl-stack) ([#195](https://github.com/egao1980/cl-stack/issues/195)).

## License

MIT — see [LICENSE](LICENSE).
