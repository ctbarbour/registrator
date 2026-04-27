## Stage 1: Build the OTP release
FROM erlang:28-alpine AS builder

RUN apk add --no-cache git

WORKDIR /build
COPY rebar.config rebar.lock ./
RUN rebar3 get-deps && rebar3 compile

COPY config/ config/
COPY src/    src/

RUN rebar3 as prod release

## Stage 2: Minimal runtime image
## Use alpine:3.23 to match erlang:28-alpine builder (Alpine 3.23.x, OpenSSL 3.5.x).
## crypto NIF is compiled against OpenSSL 3.5; older Alpine causes symbol-not-found crashes.
FROM alpine:3.23 AS runtime

RUN apk add --no-cache \
    ncurses-libs \
    openssl \
    libstdc++

COPY --from=builder /build/_build/prod/rel/registrator /opt/registrator

ENV RELX_REPLACE_OS_VARS=true

EXPOSE 5555/udp

ENTRYPOINT ["/opt/registrator/bin/registrator"]
CMD ["foreground"]
