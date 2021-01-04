{ pkgs, lib, ... }:

{
  imports = [
    ../virtualisation.nix
    ./vagrant-network.nix
    ./vagrant-users.nix
  ];
}
