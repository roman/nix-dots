{

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    homeManager.url = "github:nix-community/home-manager/release-21.05";
    homeManager.inputs.nixpkgs.follows = "nixpkgs";

    emacsOverlay.url = "github:nix-community/emacs-overlay";
    cthulhu.url = "git+ssh://git@github.internal.digitalocean.com/digitalocean/cthulhu?rev=8685cb2d6885b28b0d8f64f20b46f5e8133bb958&ref=nixpkgs-compute";
  };

  outputs = inputs@{ self, nixpkgs, darwin, homeManager, emacsOverlay, cthulhu }:

    let
      homeModules = import ./config/home-manager inputs;
      hm = import ./lib/hm.nix;
      vagrant = import ./lib/vagrant.nix inputs;
      flakeOverlay = final: prev: {
        keytar = import ./nix/keytar prev;
      };

    in {

      darwinConfigurations.westeros = darwin.lib.darwinSystem {
        modules = [
          homeManager.darwinModules.home-manager
          {
            home-manager.useUserPackages = true;
          }
          ({ pkgs, ... }: {
            home-manager.users.roman = hm.buildHomeManagerConfig {
              modules = with homeModules; [
                bash
                emacs
                git
                nix-utils
              ];
              overlays = [
                emacsOverlay.overlay
              ];
            };
          })
        ];

        packages = {
          inherit (cthulhu.outputs.packages.x86_64-linux) orca-tools;
        };


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
    };
}
