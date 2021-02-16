{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    homeManager.url = "github:nix-community/home-manager/release-20.09";
    homeManager.inputs.nixpkgs.follows = "nixpkgs";

    emacsOverlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = inputs@{ self, nixpkgs, homeManager, emacsOverlay }:
    let

      homeModules = import ./config/home-manager inputs;
      zooLib = import ./lib inputs;

    in {

      homeManagerConfigurations = {
        # normally ubuntu and vagrant are names found in vagrant images

        ubuntu = zooLib.buildVagrantGuestOS {
          username = "ubuntu";
          modules = homeModules;
          overlays = [ emacsOverlay.overlay ];
        };

        vagrant = zooLib.buildVagrantGuestOS {
          username = "vagrant";
          modules = homeModules;
          overlays = [ emacsOverlay.overlay ];
        };

      };

      nixosConfigurations = {
        nixbox = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            (args: { networking.hostName = "nixbox"; })
            homeManager.nixosModules.home-manager
            (import ./config/os/vagrant {
              overlays = [ emacsOverlay.overlay ];
              modules = homeModules;
            })
            ./config/os/nix-flakes
            ./config/os/docker
          ];
        };
      };
    };
}
