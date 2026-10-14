# Saline

Saline is a dormant research prototype containing a chain node, wallet, block explorer, and intent editor. It is released as is under [Apache 2.0](LICENSE). No maintenance, support, or security updates are promised.

The code includes public, deterministic demo/testnet wallet phrases, a testnet faucet key, and a development auto-unlock password. These are reproducible test fixtures, not secure credentials. Never use them with real funds. Security-related TODOs document unfinished work.

See [THIRD_PARTY_NOTICES.md](THIRD_PARTY_NOTICES.md) for dependency licenses, the bundled-code inventory, and historical provenance limitations. The [Saline SDK](https://github.com/risingsealabs/saline-sdk) retains its MIT license. Historical workflows in [docs/legacy-workflows](docs/legacy-workflows) are disabled.

## Build status

On 2026-10-15, `nix-shell --run 'make dev'` started the local devnet on aarch64 macOS with GHC 9.8.4. Both node RPC endpoints responded and produced blocks, and the UI served at `http://localhost:8000/`. The full suite passed all 21 tests, including network transfers, swaps, intents, node joining, and metrics. Browser onboarding reached password setup; a complete browser wallet transaction and packaged web/mobile builds remain unverified.

### Limits of this snapshot

Application persistence and recovery are unfinished or unverified, and the node includes a test faucet. Browser wallet protection is prototype-grade: the GHCJS path uses SHA-256 password derivation and XOR secret protection. These need further work before any use with real assets. The [June 2024 whitepaper](saline_whitepaper.pdf) describes the intended design, including features that may not be implemented in this snapshot.

Forks are welcome. Historical plans and contact details do not imply future development or a response from the original team.

### hsblst pin and upstream compatibility

Saline pins [hsblst commit `4ac103247a30a0c8624bae9acec026a2ed4ad997`](https://github.com/serokell/hsblst/commit/4ac103247a30a0c8624bae9acec026a2ed4ad997), originally from `alexfmpe/hsblst`. That fork was unavailable during the 2026-10-15 release checks, so [deps.nix](deps.nix) now fetches the exact commit through `serokell/hsblst`. The revision, source hash, and BLST submodule are unchanged; Nix successfully verified the recovered source.

This is a source-location change, not an upgrade to an upstream release. The pin contains two fork commits: EIP-2333 key-derivation exports and GHC 8.10 compatibility work. Neither commit was in upstream `master`'s history when checked. Saline uses `deriveMasterEIP2333` and `deriveChildEIP2333`, which were absent from the current upstream public API.

Keep this exact pin unless a replacement has been checked for those APIs, wallet key-derivation compatibility, and the development and legacy web/mobile compiler paths. Successful source retrieval alone does not establish that the full build or devnet works.

## Project structure

| Directory | Purpose |
| --- | --- |
| [common](common) | Shared types, transaction schemas, cryptography, and utilities |
| [saline-node](saline-node) | Chain node and application handlers |
| [frontend](frontend) | Wallet, explorer, and intent editor UI |
| [backend](backend) | Web server serving the app; no application API or database |
| [dev](dev) | Local network, development server, and tests |
| [saline-sdk](https://github.com/risingsealabs/saline-sdk) | Python SDK, pinned as a Git submodule |

The node uses Tendermint/CometBFT through Kepler libraries. The UI uses Haskell, Reflex, and Obelisk, with a browser or webview on each platform. Non-web builds run the Haskell code natively through JSaddle.

## Local development

Install [Nix](https://nixos.org/download/) and run these commands from the repository root:

```sh
nix-shell
make dev
```

The shell supplies GHC, Cabal, HLS, Tendermint, and development tools. A separate Obelisk `ob` installation is not needed. Use this shell for Cabal commands: [cabal.project](cabal.project) disables package repositories and relies on dependencies supplied by Nix.

`make dev` runs the handler-level tests, then starts a temporary local network and serves the UI at <http://localhost:8000/>. It watches source and configuration changes. The historical workflow had a [Firefox/JSaddle issue](https://github.com/ghcjs/jsaddle/issues/64); browser compatibility has not been rechecked.

Run the following inside `nix-shell`:

| Command | Purpose |
| --- | --- |
| `make build` | Compile all enabled components |
| `make repl` | Open a REPL with the project packages |
| `make watch` | Reload the REPL on source changes |
| `make test` | Watch and rerun serialization and handler-level tests; excludes network tests |
| `make dev` | Run the local network and UI after handler-level tests pass |
| `make hoogle` | Serve local Haskell API documentation at <http://localhost:8080/> |

Stop `make dev` before running the full test suite: both use the same local ports. Inside `nix-shell`, run the suite once, including tests that launch temporary Tendermint nodes:

```sh
cabal test dev:test:dev --test-show-details=direct
```

This selects the suite defined in [dev/dev.cabal](dev/dev.cabal); see [Saline.Test](dev/src/Saline/Test.hs) for the scenarios. A successful Nix build does not establish that these tests passed: the Nix `dev` derivation disables test execution. See the [Cabal test reference](https://cabal.readthedocs.io/en/stable/cabal-commands.html#cabal-test) for CLI options.

### Configuration and assets

The checked-in configuration points to the local development network. See [config/readme.md](config/readme.md) for file meanings and visibility.

Place assets in [static](static). Reference them with `$(static "path/to/file")`, as in [Saline.App.Assets](frontend/src/Saline/App/Assets.hs). Existing asset edits are served on browser refresh. After adding, removing, or renaming assets, exit and re-enter `nix-shell`, then restart the development command and editor's Haskell language server. This also addresses compile-time errors reporting missing files in the Nix store's `static` directory.

The shared UI loads fonts and scripts from external CDNs as well as bundled assets. Native builds also use this UI; they are not limited to contacting chain nodes.

### Transactions and SDK bindings

Use the transaction types in [Saline.Network.Node.Transaction](common/src/Saline/Network/Node/Transaction.hs) and the pinned [Saline SDK](https://github.com/risingsealabs/saline-sdk). Serialization roundtrip tests are in [Saline.Test](dev/src/Saline/Test.hs).

The SDK is optional for core development. To work on its generated bindings, initialize the pinned checkout from the repository root:

```sh
git submodule update --init --recursive saline-sdk
```

Follow the SDK's own setup instructions. The historical `make sdk-python` helper runs inside `nix-shell` and requires Poetry, which is not included in the shell. It overwrites SDK bindings and roundtrip tests, then invokes pytest. The helper ignores pytest's exit status, so run the SDK tests separately to verify the result. See [the generator](common/src/Saline/Integrations/Python.hs).

## Build artifacts

Run these from the repository root, outside the development shell. The attribute name `production` is historical; it does not imply production readiness.

| Artifact | Command |
| --- | --- |
| Node executable | `nix-build project.nix -A production.saline-node` |
| Web server and compiled UI | `nix-build project.nix -A production.webapp` |
| Node Docker image | `nix-build project.nix -A cd.saline-node` |
| Web Docker image | `nix-build project.nix -A cd.webapp` |

These targets require a Linux builder. On macOS, configure a Linux builder for the corresponding architecture. The `cd` image targets include debug tools.

[project.nix](project.nix) defaults to GHC 9.8 for development and the node. Web and mobile builds use the older Obelisk/GHCJS toolchain in [default.nix](default.nix), so they do not share the same compiler or dependency set.

Historical mobile targets are `make android` (x86_64 Linux builder) and `make iOS` (x86_64 macOS builder). They are unverified and still use example application identifiers in `default.nix`. Signing, distribution, and desktop packaging are not documented.

## UI development references

- [Reflex tutorial](https://reflex-frp.org/tutorial)
- [Obelisk routing](https://github.com/obsidiansystems/obelisk/blob/master/lib/route/docs/introduction.md)
- [JSaddle's native/browser model](https://github.com/ghcjs/jsaddle#how-does-it-work-on-ghc)
- [Reflex quick reference](https://github.com/reflex-frp/reflex/blob/develop/Quickref.md)
- [Reflex DOM quick reference](https://github.com/reflex-frp/reflex-dom/blob/develop/Quickref.md)
