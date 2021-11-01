FROM alpine:3.14

RUN wget -O- "https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz" | gunzip -c > /usr/local/bin/elm
RUN chmod +x /usr/local/bin/elm

WORKDIR /build

COPY src /build/src
COPY assets /build/assets
COPY elm.json index.html /build/

RUN elm make src/Main.elm --output elm.js


FROM nginx:latest

COPY --from=0 /build/elm.js /build/index.html /usr/share/nginx/html/
COPY --from=0 /build/assets /usr/share/nginx/html/assets
