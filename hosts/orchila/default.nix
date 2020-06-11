{ modulesPath, lib, ... }:
let
  do-userdata     = lib.optional (builtins.pathExists ./do-userdata.nix) ./do-userdata.nix;
  do-nixos-module = modulesPath + "/virtualisation/digital-ocean-config.nix";
  dots-system   = ../../system;
in
  {
    imports =  do-userdata ++ [do-nixos-module dots-system];
  }
