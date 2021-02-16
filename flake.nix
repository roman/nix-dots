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
      vagrant = import ./lib/vagrant inputs;

    in {

      homeManagerConfigurations = {
        # normally ubuntu and vagrant are names found in vagrant images

        ubuntu = vagrant.buildVagrantUbuntu {
          username = "ubuntu";
          modules = homeModules;
          overlays = [ emacsOverlay.overlay ];
        };

        vagrant = vagrant.buildVagrantUbuntu {
          username = "vagrant";
          modules = homeModules;
          overlays = [ emacsOverlay.overlay ];
        };

      };

      nixosConfigurations = {
        nixbox = vagrant.buildVagrantNixOS {
          hostname = "nixbox";
          overlays = [ emacsOverlay.overlay ];
          modules = homeModules;
        };
      };
    }

}
