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
        { system, ... }:
        let
          emacs-ci-pkgs = inputs.emacs-ci.packages.${system};
          overlay = final: prev: {
            emacs = emacs-ci-pkgs.emacs-30-1;
          };
          pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [ overlay ];
          };
          emacsVersions = {
            "27-2" = emacs-ci-pkgs.emacs-27-2;
            "28-2" = emacs-ci-pkgs.emacs-28-2;
            "29-4" = emacs-ci-pkgs.emacs-29-4;
            "30-1" = emacs-ci-pkgs.emacs-30-1;
            snapshot = emacs-ci-pkgs.emacs-snapshot;
          };
        in
        {
          treefmt = {
            programs.nixfmt.enable = true;
          };

          checks = pkgs.lib.mapAttrs (
            version: emacs:
            pkgs.runCommand "lazy-el-test-${version}"
              {
                buildInputs =
                  (with pkgs; [
                    gnumake
                  ])
                  ++ [ emacs ];
              }
              ''
                cp -r ${./.} source
                cd source
                chmod -R u+w .
                make test
                make compile
                touch $out
              ''
          ) emacsVersions;

          devShells.default = pkgs.mkShell {
            packages = with pkgs; [
              emacs
              gnumake
            ];
          };
        };
    };
}
