{
  description = "Blueprint";
  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nixpkgs, utils, fenix, ... }: utils.lib.eachDefaultSystem (system:
  let
    pkgs = nixpkgs.legacyPackages.${system};
  in
  {
    devShells = {
      default = pkgs.mkShell {
        name = "blueprint";
        packages = with pkgs; [
          cargo
          rustc
          clippy
          rustfmt
          just
          rust-analyzer
          cargo-unused-features
          cargo-bloat
          cargo-flamegraph
          hyperfine
          valgrind
          nixpkgs-fmt
          nil
        ];
      };
      nightly = pkgs.mkShell {
        name = "blueprint-nightly";
        packages = with pkgs; [
          (fenix.packages.x86_64-linux.complete.withComponents [
            "cargo"
            "rustc"
            "rust-src"
          ])
          just
        ];
      };
    };

    packages.default = pkgs.rustPlatform.buildRustPackage {
      pname = "blueprint-scheme";
      version = "0.1.0";
      cargoLock.lockFile = ./Cargo.lock;
      src = ./.;
    };
  });
}
