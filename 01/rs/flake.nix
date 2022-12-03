{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    alejandra.url = "github:kamadorueda/alejandra/3.0.0";
  };
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    flake-compat,
    alejandra,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in rec {
        formatter = alejandra.defaultPackage.${system};
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            rustup
            cargo
            cargo-generate
          ];
          RUSTC_VERSION =  pkgs.lib.readFile ./rust-toolchain;
          LIBCLANG_PATH = pkgs.lib.makeLibraryPath [pkgs.llvmPackages_latest.libclang.lib];
          HISTFILE = toString ./.history;
          shellHook = ''
            export PATH=$PATH:''${CARGO_HOME:-~/.cargo}/bin
            export PATH=$PATH:''${RUSTUP_HOME:-~/.rustup}/toolchains/$RUSTC_VERSION-x86_64-unknown-linux-gnu/bin/
          '';
        };
      }
    );
}
