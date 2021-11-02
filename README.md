# stignore-manager
Web interface for interacting with many stignore agents

Written in [Elm](https://elm-lang.org/)

## Development
### Install
1. [Install Elm](https://guide.elm-lang.org/install/elm.html)

### Run
1. Compile `elm make src/Main.elm --output elm.js`
2. Run `docker run -it --rm --name stignore-manager -p 8000:80 -v $(pwd):/usr/share/nginx/html:ro nginx`
3. Access at http://localhost:8000

## Production
0. (Optional) Build image `docker build -t mytag:latest .`
1. Run docker image from `ghcr.io/dalmura/stignore-manager:latest` or your own registry
