# A shell setup providing build tools and utilities for a development
# environment. This is now based on haskell.nix and it's haskell-nix.project
# (see 'default.nix').
{ compiler ? "ghc8107"
  # nixpkgs 21.11
, pkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/a7ecde854aee5c4c7cd6177f54a99d2c1ff28a31.tar.gz") { }

, hsPkgs ? import ./default.nix { }

, withoutDevTools ? false

, libsodium-vrf ? pkgs.libsodium.overrideAttrs (oldAttrs: {
    name = "libsodium-1.0.18-vrf";
    src = pkgs.fetchFromGitHub {
      owner = "input-output-hk";
      repo = "libsodium";
      # branch tdammers/rebased-vrf
      rev = "66f017f16633f2060db25e17c170c2afa0f2a8a1";
      sha256 = "12g2wz3gyi69d87nipzqnq4xc6nky3xbmi2i2pb2hflddq8ck72f";
    };
    nativeBuildInputs = [ pkgs.autoreconfHook ];
    configureFlags = "--enable-static";
  })

, # Add cardano-node & cardano-cli for our shell environment.
  # This is stable as it doesn't mix dependencies with this code-base; the
  # fetched binaries are the "standard" builds that people test. This should be
  # fast as it mostly fetches Hydra (CI) caches without building much.
  cardano-node ? import
    (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-node";
      rev = "1.34.1";
      sha256 = "1hh53whcj5y9kw4qpkiza7rmkniz18r493vv4dzl1a8r5fy3b2bv";
    })
    { }
}:
let
  libs = [
    libsodium-vrf
    pkgs.glibcLocales
    pkgs.zlib
    pkgs.lzma
  ]
  ++
  pkgs.lib.optionals (pkgs.stdenv.isLinux) [ pkgs.systemd ];

  buildInputs = [
    pkgs.git
    pkgs.pkgconfig
    pkgs.haskellPackages.hspec-discover
    pkgs.haskellPackages.cabal-plan
    # For validating JSON instances against a pre-defined schema
    pkgs.python3Packages.jsonschema
    # For plotting results of hydra-cluster benchmarks
    pkgs.gnuplot
    # For integration tests
    cardano-node.cardano-node
  ];

  devInputs = if withoutDevTools then [] else [
    # The interactive Glasgow Haskell Compiler as a Daemon
    pkgs.haskellPackages.ghcid
    # Generate a graph of the module dependencies in the "dot" format
    pkgs.haskellPackages.graphmod
    # Automagically format .cabal files
    pkgs.haskellPackages.cabal-fmt
    # Handy to interact with the hydra-node via websockets
    pkgs.ws
    # Like 'jq' to manipulate JSON, but work for YAML
    pkgs.yq
    # For docs/ (i.e. Docusaurus, Node.js & React)
    pkgs.yarn
    # To interact with cardano-node and testing out things
    cardano-node.cardano-cli
  ];

  # Haskell.nix managed tools (via hackage)
  buildTools = {
    cabal = "3.4.0.0";
  };

  devTools = if withoutDevTools then {} else {
    fourmolu = "0.4.0.0"; # 0.5.0.0 requires Cabal 3.6
    haskell-language-server = "latest";
  };

  haskellNixShell = hsPkgs.shellFor {
    # NOTE: Explicit list of local packages as hoogle would not work otherwise.
    # Make sure these are consistent with the packages in cabal.project.
    packages = ps: with ps; [
      marlowe-hydra-poc
    ];

    tools = buildTools // devTools;

    buildInputs = libs ++ buildInputs ++ devInputs;

    withHoogle = !withoutDevTools;

    # Always create missing golden files
    CREATE_MISSING_GOLDEN = 1;

    # Force a UTF-8 locale because many Haskell programs and tests
    # assume this.
    LANG = "en_US.UTF-8";

    GIT_SSL_CAINFO = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
  };

  # A "cabal-only" shell which does not use haskell.nix
  cabalShell = pkgs.mkShell {
    name = "hydra-node-cabal-shell";

    buildInputs = libs ++ [
      pkgs.haskell.compiler.${compiler}
      pkgs.cabal-install
      pkgs.pkgconfig
    ] ++ buildInputs ++ devInputs;

    # Ensure that libz.so and other libraries are available to TH splices.
    LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath libs;

    # Force a UTF-8 locale because many Haskell programs and tests
    # assume this.
    LANG = "en_US.UTF-8";

    # Make the shell suitable for the stack nix integration
    # <nixpkgs/pkgs/development/haskell-modules/generic-stack-builder.nix>
    GIT_SSL_CAINFO = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
    STACK_IN_NIX_SHELL = "true";
  };

in
haskellNixShell // { cabalOnly = cabalShell; }
