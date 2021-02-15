{ pkgs, lib, ... }:

{
  imports = [
    ./vagrant-network.nix
    ./vagrant-users.nix
    ./vagrant-services.nix
    ./vagrant-hardware.nix
  ];
}
