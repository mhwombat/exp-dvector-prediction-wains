let 
  pkgs = import <nixpkgs> { };
in 
  pkgs.haskellPackages.developPackage {
    root = ./.;
    source-overrides = {
      som = ../som;
      creatur = ../creatur;
      creatur-wains = ../creatur-wains;
      creatur-wains-test-utils = ../creatur-wains-test-utils;
      creatur-dvector-wains = ../creatur-dvector-wains;
      gray-extended = ../gray-extended;
      grid = ../grid;
    };
  }
