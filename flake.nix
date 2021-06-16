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
      vagrant = import ./lib/vagrant.nix inputs;

    in {

      homeManagerConfigurations = {
        # normally ubuntu and vagrant are names found in vagrant images

        ubuntu = vagrant.buildManagedUser {
          username = "ubuntu";
          homeDirectory = "/home/ubuntu";
          modules = with homeModules; [
            bash
            emacs
            git
            nix-utils
          ];
          overlays = [ emacsOverlay.overlay ];
        };

        roman = vagrant.buildManagedUser {
          username = "roman";
          homeDirectory = "/home/roman";
          system = "x86_64-linux";
          modules = with homeModules; [
            bash
            emacs
            git
            nix-utils
            ui
          ];
          overlays = [ emacsOverlay.overlay ];
        };

        vagrant = vagrant.buildManagedUser {
          username = "vagrant";
          modules = with homeModules; [
            bash
            emacs
            git
            nix-utils
          ];
          overlays = [ emacsOverlay.overlay ];
        };

      };

      nixosConfigurations = {
        nixbox = vagrant.buildVagrantNixOS {
          hostname = "nixbox";
          overlays = [ emacsOverlay.overlay ];
          modules = with homeModules; [
            bash
            emacs
            git
            nix-utils
          ];
        };
      };

    };

}
