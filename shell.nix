{ system ? builtins.currentSystem
, pkgs ? import <nixpkgs> { inherit system; config = {}; overlays = []; }
}:
pkgs.mkShell {
  buildInputs = with pkgs; [ cabal2nix ];
}
                         
