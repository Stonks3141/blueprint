{
  description = "Blueprint";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    utils.url = "github:gytis-ivaskevicius/flake-utils-plus";
  };
  outputs = inputs@{ self, nixpkgs, utils, ... }: utils.lib.mkFlake {
    inherit self inputs;
    outputsBuilder = channels:
      let pkgs = channels.nixpkgs; in {
        devShell = pkgs.mkShell {
          name = "blueprint";
          packages = with pkgs; [
            cargo
            rustc
            clippy
            rustfmt
            rust-analyzer
            cargo-flamegraph
            hyperfine
            nixpkgs-fmt
            nil
          ];
        };
      };
  };
}
