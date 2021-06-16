{ homeManager, nixpkgs, ... }:

let

  hm = import ../lib/hm.nix;

in

{

  # buildManagedUser is a function that creates a configuration that is
  # vagrant guest friendly on an Ubuntu OS
  buildManagedUser = { username, homeDirectory, system, modules, overlays, ... }:
    homeManager.lib.homeManagerConfiguration {
      inherit username homeDirectory system;
      configuration = hm.buildHomeManagerConfig { inherit modules overlays; };
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
