{
  description = "Blueprint";
  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    utils.url = "github:gytis-ivaskevicius/flake-utils-plus";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = inputs@{ self, nixpkgs, utils, fenix, ... }: utils.lib.mkFlake {
    inherit self inputs;
    sharedOverlays = [ fenix.overlays.default ];
    outputsBuilder = channels:
      let pkgs = channels.nixpkgs; in {
        devShells = {
          default = pkgs.mkShell {
            name = "blueprint";
            packages = with pkgs; [
              cargo
              rustc
              clippy
              rustfmt
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
                "clippy"
                "rustfmt"
                "rust-src"
              ])
              rust-analyzer-nightly
              cargo-unused-features
              cargo-bloat
              cargo-flamegraph
              hyperfine
              nixpkgs-fmt
              nil
            ];
          };
        };
      };
  };
}
