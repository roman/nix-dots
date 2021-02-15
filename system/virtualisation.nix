{ lib, config, pkgs, ... }:

{
  # Enable Docker
  virtualisation = {
    docker = {
      enable = true;
      enableOnBoot = false;
    };
  };

  environment.systemPackages =
    lib.mkIf config.virtualisation.docker.enable [ pkgs.docker-compose ];
}
