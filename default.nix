{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.callCabal2nix "exp-dvector-prediction-wains" ./. {}
