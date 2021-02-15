packages:
{ pkgs, lib, ... }:

{
  imports = [
    (import ./vagrant-users.nix packages)
    ./vagrant-services.nix
    ./vagrant-hardware.nix
  ];
}
