{ pkgs, lib, config, ... }:

{
  imports = [
    ./base.nix
    ./git.nix 
  ];
}
