# llm-protocol-openai

OpenAI-compatible HTTP backend for [`llm-protocol`](https://github.com/egao1980/llm-protocol): `POST /chat/completions` (`generate`) and `POST /responses` (`respond`). Not the protocol.

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
   (stack-llm:respond b "ping" :settings '(:temperature 0 :max-tokens 32))))
```

`OPENAI_API_KEY` / `LM_API_TOKEN` / `OPENAI_BASE_URL` / `OPENAI_MODEL` fill omitted initargs. Default base is LM Studio `http://127.0.0.1:1234/v1`. Wave-1 `stream-generate` / `stream-respond` → `llm-unsupported`.

Tests: mock `request-fn` + a usocket fixture through async×libuv. Live LM Studio: `LLM_OPENAI_LIVE=1`. CI needs published `llm-protocol` on GHCR.

Part of [cl-stack](https://github.com/egao1980/cl-stack) ([#195](https://github.com/egao1980/cl-stack/issues/195)).

## License

MIT — see [LICENSE](LICENSE).
