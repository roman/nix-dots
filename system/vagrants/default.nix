{ pkgs, lib, ... }:

# with import ../../lib;

let
  sources = import ../../nix/sources.nix;
in
  {
    imports = [ 
      "${sources.home-manager}/nixos" 
      ../../modules  
      ./vagrant-hostname.nix
      ./vagrant-network.nix
      ./vagrant-users.nix
    ];
  
  }
