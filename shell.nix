let
  nixpkgs = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/13b0f9e6ac78abbbb736c635d87845c4f4bee51b.tar.gz";
    sha256 = "0js8a1abl5mi1j0l9wiasis8srm1a7wjrd3scwb8xpq3i0amvz8r";
  };
  overlay = final: prev: {
    emacs = prev.emacs30;
  };
  pkgs = import nixpkgs {
    overlays = [overlay];
  };
in

pkgs.mkShell {
  packages = with pkgs; [
    emacs
    gnumake
  ];
}
