{ compiler ? "ghc8107"

, system ? builtins.currentSystem

, haskellNix ? import
    (builtins.fetchTarball
      "https://github.com/input-output-hk/haskell.nix/archive/28dbf2f4bd32a4fbd1a2e9de45d02ad977b062d9.tar.gz")
    { }

, iohkNix ? import
    (builtins.fetchTarball
      "https://github.com/input-output-hk/iohk-nix/archive/5e667b374153327c7bdfdbfab8ef19b1f27d4aac.tar.gz")
    { }

  # nixpkgs-unstable as also used by cardano-node, cardano-ledger et al
, nixpkgsSrc ? builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/1882c6b7368fd284ad01b0a5b5601ef136321292.tar.gz"
}:
let
  pkgs = import nixpkgsSrc (haskellNix.nixpkgsArgs // {
    overlays =
      # Haskell.nix (https://github.com/input-output-hk/haskell.nix)
      haskellNix.overlays
        # needed for cardano-crypto-class which uses a patched libsodium
        ++ iohkNix.overlays.crypto;
  });
  hsPkgs = pkgs.haskell-nix.project {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "hydra-poc";
      src = ./.;
    };
    projectFileName = "cabal.project";
    compiler-nix-name = compiler;

    modules = [{
      # https://github.com/input-output-hk/cardano-wallet/commit/ced95e1b84ce8d9faa53268be45e96701ccc16e9
      packages.cardano-config.components.library.build-tools = [ pkgs.buildPackages.buildPackages.gitMinimal ];

      # https://github.com/input-output-hk/iohk-nix/pull/488
      packages.cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
      packages.cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];

      # Broken due to haddock errors. Refer to https://github.com/input-output-hk/plutus/blob/master/nix/pkgs/haskell/haskell.nix
      # hydra-plutus.doHaddock = false;
      packages.plutus-core.doHaddock = false;
      packages.plutus-ledger.doHaddock = false;
      packages.marlowe-actus.doHaddock= false;
      packages.marlowe.doHaddock = false;
      packages.cardano-wallet-core.doHaddock= false;
      packages.marlowe-cli.doHaddock= false;
      packages.plutus-script-utils.doHaddock = false;
      packages.hydra-cardano-api.doHaddock = false;
      packages.hydra-plutus.doHaddock = false;

    }];
  };
in
{
  inherit pkgs hsPkgs compiler;
}
