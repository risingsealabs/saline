# Configuration

Configuration uses text files read through Obelisk. The checked-in values target the local development network.

| File | Contents |
| --- | --- |
| [common/route](common/route) | App URL, initially `http://localhost:8000` |
| [frontend/node](frontend/node) | One or more node RPC URLs, one per line; initially ports 26657 and 26667 on `127.0.0.1` |
| [common/watcher](common/watcher) | Encoded watcher **public** key used by the demo network; not a private signing key |

Files under `common/` and `frontend/` are available to the client. Never put secrets there. Server-only configuration belongs under `backend/`; keep any real credentials out of Git.

When accessing the development app from another device, both the app URL and node RPC URLs must be reachable from that device. `localhost` and `127.0.0.1` refer to the device itself. Changing only `common/route` is insufficient.

`make dev` watches configuration changes. Native app builds embed configuration, so rebuild them after changing it.
