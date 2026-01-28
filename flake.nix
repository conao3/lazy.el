{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    emacs-ci.url = "github:purcell/nix-emacs-ci";
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      imports = [
        inputs.treefmt-nix.flakeModule
      ];

      perSystem =
        { pkgs, system, ... }:
        let
          emacs-ci-pkgs = inputs.emacs-ci.packages.${system};
          emacsVersions = {
            "27-2" = emacs-ci-pkgs.emacs-27-2;
            "28-2" = emacs-ci-pkgs.emacs-28-2;
            "29-4" = emacs-ci-pkgs.emacs-29-4;
            "30-1" = emacs-ci-pkgs.emacs-30-1;
            snapshot = emacs-ci-pkgs.emacs-snapshot;
          };

          makeCheck =
            version: emacs:
            pkgs.runCommand "lazy-el-test-${version}"
              {
                buildInputs = [
                  emacs
                  pkgs.gnumake
                ];
              }
              ''
                cp -r ${./.} source
                cd source
                chmod -R u+w .
                make test
                make compile
                touch $out
              '';
        in
        {
          treefmt = {
            programs.nixfmt.enable = true;
          };

          checks = pkgs.lib.mapAttrs makeCheck emacsVersions;

          devShells.default = pkgs.mkShell {
            packages = [
              emacsVersions."30-1"
              pkgs.gnumake
            ];
          };
        };
    };
}
