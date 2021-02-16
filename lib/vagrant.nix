{ homeManager, nixpkgs, ... }:

let

  lib = import ../lib;

in

{

  # buildVagrantUbuntu is a function that creates a configuration that is
  # vagrant guest friendly on an Ubuntu OS
  buildVagrantUbuntu = { modules, overlays, ... }:
    homeManager.lib.homeManagerConfiguration {
      username = "vagrant";
      system = "x86_64-linux";
      configuration = lib.buildHomeManagerConfig { inherit modules overlays; };
    };

  # buildVagrantNixOS is a function that creates a configuration that is vagrant
  # guest friendly on a NixOS OS
  buildVagrantNixOS = { hostname, modules, overlays, ... }:
    nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        (_: { networking.hostName = hostname; })
        homeManager.nixosModules.home-manager
        (import ../config/os/vagrant {
          overlays = overlays;
          modules = modules;
        })
        ../config/os/nix-flakes
        ../config/os/docker
      ];
    };
}
