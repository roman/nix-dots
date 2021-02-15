args:
{ pkgs, lib, ... }:

{
  imports = [
    (import ./vagrant-users.nix args)
    ./vagrant-services.nix
    ./vagrant-hardware.nix
  ];
}
