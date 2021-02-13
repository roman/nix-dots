flakeInputs:
{ lib, config, pkgs, ... }:

{
  # Enable Docker
  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
  };

  home.packages =
    lib.mkIf config.virtualisation.docker.enable [ pkgs.docker-compose ];
}
