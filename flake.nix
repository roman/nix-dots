{

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    homeManager.url = "github:nix-community/home-manager/release-21.05";
    homeManager.inputs.nixpkgs.follows = "nixpkgs";

    emacsOverlay.url = "github:nix-community/emacs-overlay/f6768d390c0c6033735a7538150131fea8518ad6";
  };

  outputs = inputs@{ self, nixpkgs, homeManager, emacsOverlay }:

    let

      homeModules = import ./config/home-manager inputs;
      vagrant = import ./lib/vagrant.nix inputs;
      flakeOverlay = final: prev: {
        keytar = import ./nix/keytar prev;
      };

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
          overlays = [ emacsOverlay.overlay flakeOverlay ];
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
