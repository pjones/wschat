{ pkgs ? (import <nixpkgs> {}).pkgs }:
pkgs.haskellPackages.callPackage ./wschat.nix { }
