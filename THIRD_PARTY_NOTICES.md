# Third-party components

The root Apache 2.0 license applies to project code. Independently licensed dependencies and copied source retain their own terms. Original embedded notices remain in the source and bundled JavaScript.

| Component | License | Notice |
| --- | --- | --- |
| saline-sdk at `00c6812ee2d0fe4a7f690533432bc33e60b430ba` | MIT | [SDK license](third-party-licenses/saline-sdk.txt); public submodule |
| Copied Obelisk route utilities in `common/src/Saline/Utils.hs` | BSD-3-Clause | [Obelisk license](third-party-licenses/obelisk-route.txt), from the exact revision cited in the source |
| @chainsafe/bls-hd-key 0.3.0 | Apache-2.0 | [License](third-party-licenses/chainsafe-bls-hd-key-0.3.0.txt) |
| @noble/curves 1.6.0 | MIT | [License](third-party-licenses/noble-curves-1.6.0.txt) |
| @noble/hashes | MIT | [License](third-party-licenses/noble-hashes-1.5.0.txt); matching sources recorded in the bundle inventory |
| bip39 3.1.0 | ISC | [License](third-party-licenses/bip39-3.1.0.txt) |
| tendermint 5.0.2 and bundled transitive packages | MIT and other permissive licenses | [Collected package notices](third-party-licenses/javascript-bundles.txt) |
| Browserify-generated loaders and wrappers | MIT | Browserify, browser-pack and umd sections in the collected package notices |
| C and runtime code within supercop.js; Closure Library-derived conversion within hash.js | zlib, MIT/NCSA, public-domain attribution, Apache-2.0 | [Additional runtime notices](third-party-licenses/javascript-runtime-notices.txt) |
| ApexCharts 3.49.1 (CDN) | MIT | [License](third-party-licenses/apexcharts-3.49.1.txt) |

## Shipped JavaScript review

The four bundles in `static/js` were reviewed without regenerating them or changing dependencies. [The inventory](third-party-licenses/javascript-bundle-inventory.json) records their SHA-256 hashes, all 442 Browserify module entries, matching source files, package-archive URLs and npm integrity values. It accounts for 326 source-file matches, 114 deduplication references, one empty browser stub, and one embedded elliptic package-metadata module. There are 107 package identities in the module maps, plus three loader/wrapper packages covered by the notices.

Each source-file match contains the complete published source file after line-ending normalization and removal of source-map comments; Browserify's injected wrappers are outside that file. Deduplication entries identify their target module. The inventory identifies the published archives used to establish source and notice correspondence. Identical source can appear in more than one release, so these version labels are not a reconstruction of the original installation lockfile. The historical entry-package versions remain recorded in `scripts/vendor_js_libs.sh`.

The collected notices include Axios, base64-js, buffer, ieee754 and the other identified transitive packages, including licenses published in upstream README files. They cover all four bundles, not only Tendermint. Existing complete embedded notices are also retained.

## Upstream provenance limitations

Five matching packages—`int53@0.2.4`, `ndjson@1.5.0`, `old@0.1.3`, `supercop.js@2.0.1`, and `tendermint@5.0.2`—publish a license declaration but no separate complete license text in the inspected package archive or upstream repository. Their collected-notice sections reproduce the upstream author and license metadata and explicitly identify the standard declared-license terms supplied for convenience. No upstream copyright holder or year has been invented.

The release review checked each tarball against its recorded SHA-512 integrity value and compared its package declaration with the collected notice.

| Published archive | Declared license | Published author |
| --- | --- | --- |
| [int53 0.2.4](third-party-licenses/npm-archives/int53-0.2.4.tgz) | BSD-3-Clause | Danny Coates |
| [ndjson 1.5.0](third-party-licenses/npm-archives/ndjson-1.5.0.tgz) | BSD-3-Clause | max ogden |
| [old 0.1.3](third-party-licenses/npm-archives/old-0.1.3.tgz) | MIT | Matt Bell |
| [supercop.js 2.0.1](third-party-licenses/npm-archives/supercop.js-2.0.1.tgz) | ISC | 1p6 |
| [tendermint 5.0.2](third-party-licenses/npm-archives/tendermint-5.0.2.tgz) | MIT | Matt Bell |

The int53 README says “BSD”; the ndjson README names BSD-3-Clause. The package declarations identify the specific terms reproduced in the collected notices. The published supercop.js archive contains only package.json, index.js, and lib.js.

The supercop.js archive includes Emscripten-generated code from orlp/ed25519. Its build script did not pin the C source revision or compiler version. The runtime notice file preserves the upstream ed25519 zlib notice, SHA-512 attribution, and contemporary Emscripten and C-runtime notices, with their source revisions. Those revisions identify the notice sources; they are not asserted to be the unrecovered original compiler/vendor pins. The known wrapper revision is `e292dafa1e1a7127b259b806a1a5b966e4ae2f6f`. The missing pins are the orlp/ed25519 Git commit used to populate `vendor/src` and the Emscripten release or commit supplying `emcc`. This release preserves the historical bundle as supplied, rather than claiming a reproducible reconstruction of its build.

D3, Tailwind CSS, and Google Fonts are loaded remotely. ApexCharts is pinned to the MIT-licensed 3.49.1 release. The project owner has confirmed redistribution rights for project assets. Third-party marks retain their respective ownership.
