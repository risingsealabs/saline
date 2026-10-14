{ compiler ? "ghc98"
, buildPlatform ? builtins.currentSystem
, hostPlatform ? builtins.currentSystem
}:
let
  deps = import ./deps.nix;

  linuxSystem = builtins.replaceStrings [ "darwin" ] [ "linux" ] builtins.currentSystem; # aarch64-darwin -> aarch64-linux, etc

  production = {
    webapp = (import ./default.nix { system = linuxSystem; }).exe;
    saline-node = with nixpkgsDocker; haskell.lib.compose.justStaticExecutables haskell.packages.${compiler}.saline-node;
  };

  nixpkgsWith = args: import deps.pins.nixpkgs (args // {
    inherit config;
    overlays = [(import (deps.pins.obelisk + "/nixpkgs-overlays/default.nix"))];
  });

  nixpkgsEval   = nixpkgsWith { localSystem = builtins.currentSystem; };
  nixpkgsCross  = nixpkgsWith { localSystem = buildPlatform; crossSystem = hostPlatform; };
  nixpkgsDocker = nixpkgsWith {
    localSystem = linuxSystem; # Need to *build on* a linux (or builder VM)
    crossSystem = linuxSystem; # Output will *run on* a linux system
    overlays = [ dockerWithoutKVM ];
  };

  # https://github.com/NixOS/nixpkgs/issues/67079#issuecomment-920364273
  dockerWithoutKVM = final: prev:
    let
      runInLinuxVMNoKVM = drv: final.lib.overrideDerivation (final.vmTools.runInLinuxVM drv) (_: { requiredSystemFeatures = []; });
      modifiedVmTools = prev.vmTools // { runInLinuxVM = runInLinuxVMNoKVM; };
    in {
      dockerTools = prev.dockerTools.override { vmTools = modifiedVmTools; };
    };

  images = {isDebug}: with nixpkgsDocker; let mkPaths = exe: exe ++ [cacert] ++ lib.optionals isDebug [bash binutils coreutils]; in {
    webapp =
      let
        name = "webapp";

      in dockerTools.buildImage {
        inherit name;
        tag = "latest";

        copyToRoot = buildEnv {
          name = name + "-env";
          paths = mkPaths [];
          pathsToLink = ["/bin" "/etc" ];
        };

        config = {
          Entrypoint = ["backend"];
          ExposedPorts."8000/tcp" = {};
        };

        runAsRoot = ''
          cp -R ${production.webapp}/* .
          mv backend /bin
        '';
      };

    saline-node =
      let
        name = "saline-node";
      in dockerTools.buildImage {
        inherit name;
        tag = "latest";

        copyToRoot = buildEnv {
          name = name + "-env";
          paths = mkPaths [production.saline-node];
          pathsToLink = ["/bin" "/etc" ];
        };

        config = {
          EntryPoint = ["saline-node"];

          ExposedPorts."26656/tcp" = {};
          ExposedPorts."26657/tcp" = {};
          ExposedPorts."26660/tcp" = {};
        };
      };
  };

  obelisk = import deps.pins.obelisk {};
  kepler = isGhcjs: nixpkgs: import (deps.pins.kepler isGhcjs + "/nix/project.nix") { inherit nixpkgs; };
  nodeDeps = nixpkgs: with (kepler false nixpkgs); [ tendermint ];
  overrides = nixpkgs: self: super: with nixpkgs.haskell.lib.compose;
    let
      inherit (nixpkgs.stdenv) isDarwin isLinux isAarch64 isx86_64;
      when = b: f: if b then f else (x: x);

      fastBuild = x: nixpkgs.lib.pipe x [
        dontHaddock
        disableLibraryProfiling
        disableExecutableProfiling
      ];

      staticAssetsOverride =
        let
          name = "obelisk-generated-static";
          processed = (obelisk.processAssets { src = ./static; packageName = name; }).overrideAttrs (_: _: {
            # Otherwise defaults to obelisk-asset-manifest from bundled 8.10 package set and rebuilds the world
            nativeBuildInputs = [ nixpkgsEval.haskell.packages.${compiler}.obelisk-asset-manifest ];
          });
        in {
          "${name}" = self.callCabal2nix name processed.haskellManifest {};
        };

      # See "justStaticExecutables" in
      # https://github.com/NixOS/nixpkgs/blob/f73ed219335199067ae839ae4db2a32af2b879ed/doc/languages-frameworks/haskell.section.md#packaging-helpers-haskell-packaging-helpers
      fixStaticExecutables = overrideCabal (drv: {
        postInstall = ''
          remove-references-to -t ${self.warp.out} "$out/bin/wallet"
          remove-references-to -t ${self.snap-server.out} "$out/bin/wallet"
        '';
      });

    in staticAssetsOverride // {
      # Not on hackage
      cardano-crypto = self.callCabal2nix "cardano-crypto" deps.pins.cardano-crypto {};

      # Forked (TODO: upstream changes)
      hsblst = doJailbreak (self.callCabal2nix "hsblst" deps.pins.hsblst {});

      ### saline libs
      # TODO: justStaticExecutables

      frontend = nixpkgs.lib.pipe (self.callCabal2nix "frontend" ./frontend {})
        [ buildStrictly
          fastBuild
          # justStaticExecutables
        ];

      backend = nixpkgs.lib.pipe (self.callCabal2nix "backend" ./backend {})
        [ buildStrictly
          fastBuild
          # justStaticExecutables
          # fixStaticExecutables
        ];

      common = nixpkgs.lib.pipe (self.callCabal2nix "common" ./common {})
        [ buildStrictly
          fastBuild
        ];

      dev = nixpkgs.lib.pipe (self.callCabal2nix "dev" ./dev {})
        [ buildStrictly
          fastBuild
          dontCheck # tendermint runs into nix sandbox when trying to create temp dirs
        ];

      saline-node = nixpkgs.lib.pipe (self.callCabal2nix "saline-node" ./saline-node {})
        [ buildStrictly
          fastBuild
          (overrideCabal (drv: { executableSystemDepends = (drv.executableSystemDepends or []) ++ nodeDeps nixpkgs; }))
        ];

      # `security` isn't available in nixpkgs and needs be impurely taken from /usr/bin/security meaning
      # we can't run the tests unless the sandbox is disabled. If we disable the tests to make it always
      # buildable then we don't get it from nixos.org's cache (hydra CI disables the sandbox)
      # See also https://github.com/NixOS/nixpkgs/issues/303568
      # http-reverse-proxy = when isDarwin dontCheck super.http-reverse-proxy;

      ### build kepler libs with recent ghc
      hs-tendermint-client = dontCheck super.hs-tendermint-client; #TODO: needs running node

      # Not on hackage
      obelisk-executable-config-lookup = self.callCabal2nixWithOptions
        "obelisk-executable-config-lookup"
        deps.pins.obelisk
        "--subpath lib/executable-config/lookup"
        {};
    };

  config = {
    packageOverrides = nixpkgs: {
      haskell = nixpkgs.haskell // {
        packages = nixpkgs.haskell.packages // {
          "${compiler}" = nixpkgs.haskell.packages.${compiler}.override(old: {
            overrides = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions  (_: _: {}) [
              (self: super: (kepler (super.ghc.isGhcjs or false) nixpkgs).haskellOverrides self super)
              (import (deps.pins.obelisk + "/haskell-overlays/obelisk.nix"))
              (overrides nixpkgs)
            ];
          });
        };
      };
    };
  };

in rec {
  inherit deps images nixpkgsEval nixpkgsCross nixpkgsDocker production;

  cd = images { isDebug = true; };
  ci = nixpkgsEval.haskell.packages.${compiler}.dev;

  shell = nixpkgsEval.haskell.packages.${compiler}.shellFor {
    packages = p: with p; [ common backend frontend saline-node dev ];
    strictDeps = true;
    withHoogle = true;
    nativeBuildInputs = builtins.concatLists [
      (nodeDeps nixpkgsEval)
      (with nixpkgsEval.haskell.packages.${compiler}; [ haskell-language-server ])
      (with nixpkgsEval; [
        cabal-install
        ghcid
        hlint
        nixd
        nodejs
        nodePackages.browserify
        # poetry # Need poetry2
        python3
        python3Packages.pygments
        watchexec
      ])
    ];
  };
}
