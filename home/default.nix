{ pkgs, lib, config, ... }:

{
  imports = [
    ./base.nix
    ./emacs
    ./git.nix
  ];
}
