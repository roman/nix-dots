{ pkgs, lib, ... }:

with import ../../lib;

let
  sources = import ../nix/sources.nix;
in
  {
    imports = [ 
      ../modules  
      ./users.nix
    ];
  
  }
