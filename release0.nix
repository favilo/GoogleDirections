let
  pkgs = import <nixpkgs> { };

in
  pkgs.haskell.packages.ghc822.callPackage ./GoogleDirections.nix { }
