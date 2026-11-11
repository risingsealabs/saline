let
  nixpkgs = import pins.nixpkgs {};

  pins = {
    # merge of https://github.com/NixOS/nixpkgs/pull/401526
    nixpkgs = builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/a9245b8f22bb81675d374aed93736930f5109503.tar.gz";
      sha256 = "sha256:0jcnmb0smaqz7fiyzc51n2cyn9s81h3j285wv47bxifk0rvavca2";
    };

    # Branch with a bunch of PRs still unmerged upstream
    # https://github.com/risingsealabs/obelisk/tree/unmerged
    ## https://github.com/obsidiansystems/obelisk/pull/984
    ## https://github.com/obsidiansystems/obelisk/pull/988
    ## https://github.com/obsidiansystems/obelisk/pull/989
    ## https://github.com/obsidiansystems/obelisk/pull/1079
    ## https://github.com/obsidiansystems/obelisk/pull/1080
    ## https://github.com/reflex-frp/reflex-platform/pull/840
    ## https://github.com/reflex-frp/reflex-platform/pull/841
    obelisk = nixpkgs.applyPatches {
      src = nixpkgs.fetchFromGitHub {
        owner = "risingsealabs";
        repo = "obelisk";
        rev = "96cdbd718fa7f1fb22bf7e7ad41a794209a5e029";
        sha256 = "sha256-QOlc2JEEfsUOEwGPshoNrrwt66uchHj/KmQ5LMp17fw=";
      };
      patches = [./patches/obelisk-rsl-dependencies.patch];
    };

    # When we switch to 9.x ghc: https://iohk.io/en/blog/posts/2020/07/28/improving-haskells-big-numbers-support/
    cardano-crypto = nixpkgs.fetchFromGitHub {
      owner = "risingsealabs";
      repo = "cardano-crypto";
      rev = "5f111a46294986b3b9bdb93ec726792450b793bf";
      sha256 = "sha256-lGklKvrdnxwk3jx7LqE4OUjnR0Scay1ElVbwVGbAzkE=";
    };

    foundation = nixpkgs.fetchFromGitHub {
      owner = "risingsealabs";
      repo = "foundation";
      rev = "edcf3d50042cf957906a6ad3e05105f53cb1bc3d";
      sha256 = "sha256-7ye3P6kCWltKyJuHGWDXlTaqnV5TPSTTplyuW1KcUog=";
    };

    # Builds under 8.10 and exposes EIP2333. The original alexfmpe fork
    # was recovered through serokell and is preserved under risingsealabs.
    # These fork changes were not merged into upstream master as of 2026-10-15.
    # Preserve the revision/hash; see README's hsblst compatibility note before upgrading.
    hsblst = let
      wrapper = nixpkgs.fetchFromGitHub {
        owner = "risingsealabs";
        repo = "hsblst";
        rev = "4ac103247a30a0c8624bae9acec026a2ed4ad997";
        sha256 = "101ipz6q3fy61ws42sjnz52hciq4any6r3vq6hya39195vp5zxrn";
      };
      blst = nixpkgs.fetchFromGitHub {
        owner = "risingsealabs";
        repo = "blst";
        rev = "3dd0f804b1819e5d03fb22ca2e6fac105932043a";
        sha256 = "1l1c905d5dinj279vyfc8phjnl5fgihbdjgp0z20a92pwv5n7ad2";
      };
    in nixpkgs.runCommand "hsblst-source" {} ''
      mkdir -p "$out/c-source"
      cp -R ${wrapper}/. "$out/"
      chmod u+w "$out/c-source"
      cp -R ${blst}/. "$out/c-source/"
    '';

    # https://github.com/rslex/kepler/tree/nix-derivation-wip-tendermint-only
    kepler = isGhcjs: nixpkgs.applyPatches {
      src = nixpkgs.fetchFromGitHub {
        owner = "risingsealabs";
        repo = "kepler";
        rev = "5cd494f095255ed7ffe2baf734688c5140ca9c73";
        sha256 = "sha256-QSnzmjldC0BL/fcqcwK4hB5AieOJE2lZhFg38Gi0Lf0=";
      };
      patches = nixpkgs.lib.optionals isGhcjs [./patches/hs-tendermint-client-ghcjs-deps.patch];
    };
  };

  packages = self: with self; {

    # 8.10 package set: Not yet present in pinned snapshot
    base16 = callHackageDirect {
      pkg = "base16";
      ver = "1.0";
      sha256 = "sha256-pLnipLnF7YuQvCwgw7Lp7sbwhab63sdEpubeSpaoEmY=";
    } {};

    # 8.10 package set: Not yet present in pinned snapshot
    crypton = callHackageDirect {
      pkg = "crypton";
      ver = "0.34";
      sha256 = "sha256-dHvzmwq5l1dPZsp0sYFe9l8mXF/Ya5aFbkDg0ljEEKY=";
    } {};

    # 8.10 package set:
    #   Setup: Encountered missing or private dependencies:
    #   aeson >=0.7.0.5 && <2.1
    lens-aeson = callHackageDirect {
      pkg = "lens-aeson";
      ver = "1.2.3";
      sha256 = "sha256-M0+QJWxN8BCpxxJhZxXSPy5Revf9p2M9uvm4gSXdE4k=";
    } {};
  };

  patches = {};

in { inherit nixpkgs packages patches pins; }
