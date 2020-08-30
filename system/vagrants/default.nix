{ pkgs, lib, ... }:

# with import ../../lib;

let
  sources = import ../../nix/sources.nix;
in
  {
    imports = [ 
      "${sources.home-manager}/nixos" 
      ../../modules  
      ../virtualisation.nix
      ./vagrant-network.nix
      ./vagrant-users.nix
    ];
  
  }
