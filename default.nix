{ system ? builtins.currentSystem
, obelisk ? import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "16.1";

    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    config.android_sdk.accept_license = true;

    # In order to use Let's Encrypt for HTTPS deployments you must accept
    # their terms of service at https://letsencrypt.org/repository/.
    # Uncomment and set this to `true` to indicate your acceptance:
    # terms.security.acme.acceptTerms = false;
  }
}:
let
  when = b: f: if b then f else (x: x);
  deps = import ./deps.nix;
  kepler = isGhcjs: import (deps.pins.kepler isGhcjs + "/nix/project.nix") { inherit system; };
  inherit (kepler false) tendermint;

in with obelisk;
project ./. ({ pkgs, ... }: {

  # Data.Binary.Get.runGet at position 48: not enough bytes
  # CallStack (from HasCallStack):
  #   error, called at src/Data/Binary/Get.hs:351:5 in binary-0.8.8.0-8geveWiewY98oMa1Q4NvzB:Data.Binary.Get
  __closureCompilerOptimizationLevel = null; #TODO: use externs file

  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";

  packages = {
    dev = ./dev;
    saline-node = ./saline-node;
  };

  shellToolOverrides = self: super: {
    # haskell-language-server = pkgs.haskell.packages.ghc8107.haskell-language-server;

    ${if (self.ghc.isGhcjs or false) then null else "tendermint"} = tendermint;
  };

  overrides = with pkgs.haskell.lib.compose; pkgs.lib.foldr pkgs.lib.composeExtensions (_: _: {}) [
    (self: super: (kepler (super.ghc.isGhcjs or false)).haskellOverrides self super)

    (self: super: {
      inherit (deps.packages self) base16 crypton lens-aeson;
    })

    (self: super: {
      http-reverse-proxy = when pkgs.stdenv.isDarwin dontCheck super.http-reverse-proxy;
      snap = when pkgs.stdenv.isDarwin dontCheck super.snap;

      lattices = self.callHackage "lattices" "2.1" {}; # data-interval fails build due to something strange regarding lattices 2.0.3 dep on OneTuple
      cardano-crypto = self.callCabal2nix "cardano-crypto" deps.pins.cardano-crypto {};
      base16 = doJailbreak super.base16;
      entropy = self.callHackage "entropy" "0.4.1.4" {};
      hsblst = pkgs.lib.pipe (self.callCabal2nix "hsblst" deps.pins.hsblst {}) [
        doJailbreak
        (addSetupDepend self.Cabal_3_6_3_0) # Workaround some linking error with older cabal
      ];
      hs-tendermint-client = dontCheck super.hs-tendermint-client; #TODO: tests need running node
      hs-abci-types = addBuildTools [pkgs.protobuf] super.hs-abci-types; # TODO: upstream
    })

    (self: super: {
      quickcheck-instances = addBuildDepend self.text-short super.quickcheck-instances; # Dependency missing in ghcjs package set?
      math-functions = super.math-functions; # tests hang on android
      crypton = disableCabalFlag "integer-gmp" (dontCheck super.crypton); # tests hang on mobile
      cardano-crypto = disableCabalFlag "integer-gmp" (dontCheck super.cardano-crypto); # tests hang on mobile
      foundation = self.callCabal2nix "foundation" (deps.pins.foundation + "/foundation") {}; # patched cabal file
      websockets = self.callHackage "websockets" "0.12.7.3" {}; # disabled by reflex-platform ios overlay
    })

    (self: super:
      let
        fastBuild = x: pkgs.lib.pipe x [
          dontHaddock
          disableLibraryProfiling
          disableExecutableProfiling
        ];
      in {
        common   = fastBuild super.common;
        backend  = fastBuild (justStaticExecutables super.backend);
        frontend = fastBuild super.frontend;
        dev      = fastBuild super.dev;

        saline-node = pkgs.lib.pipe super.saline-node [
          dontCheck # running tendermint hits the nix sandbox
          fastBuild
          justStaticExecutables
          (overrideCabal (drv: { executableSystemDepends = (drv.executableSystemDepends or []) ++ [tendermint]; }))
        ];
      })
  ];
})
