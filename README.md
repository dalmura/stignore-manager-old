# stignore-manager
Web interface for interacting with many stignore agents

Written in [Elm](https://elm-lang.org/)

Rough feature list:
- [x] Add/Remove/Default Agents
- [x] Single depth Content Types Listing
- [ ] Multi depth Content Types Listing
- [x] Colour coded (for context) CT Listing pages
- [x] Build list of ST Ignore file actions
- [ ] Submit ST Ignore file actions
- [ ] Flush ST Ignore change to underlying folder

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

## Loading Agents
You can optionally make a /agents.json available on the same hostname to load a list of default agents:
```
wget http://localhost:8000/agents.json

{
  "agents": [
    {
      "name": "Agent #1",
      "host": "https://agent-1.mydomain.com"
    },
    {
      "name": "Agent #2",
      "host": "https://agent-2.mydomain.com"
    }
  ]
}

```
