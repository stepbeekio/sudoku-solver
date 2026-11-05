{
  description = "stepbeekio/sudoku-solver development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        jdk = pkgs.jdk21;
        clojure = pkgs.clojure.override { inherit jdk; };
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            jdk
            clojure
            pkgs.clj-kondo
            pkgs.clojure-lsp
            pkgs.just
          ];

          JAVA_HOME = jdk.home;
        };

        formatter = pkgs.nixpkgs-fmt;
      });
}
