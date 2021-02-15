{ lib, config, pkgs, ... }:

{
  # Enable Docker
  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
  };

  # Install docker-compose if docker is enabled
  environment.systemPackages =
    lib.mkIf config.virtualisation.docker.enable [ pkgs.docker-compose ];
}
