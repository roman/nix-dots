{

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    homeManager.url = "github:nix-community/home-manager";
    homeManager.inputs.nixpkgs.follows = "nixpkgs";

    emacs.url = "github:nix-community/emacs-overlay";
  }; 


  outputs = { self, nixpkgs, homeManager, emacs }: {
    homeManagerConfigurations = {
      ubuntu = homeManager.lib.homeManagerConfiguration {
        system = "x86_64-linux";
        homeDirectory = "/home/ubuntu";
        username = "ubuntu";

        configuration = { pkgs, lib, ... }: {
          imports = [ ./home ];
          nixpkgs = {
            overlays = [ emacs.overlay ];
            config = { allowUnfree = true; };
          };
        };
      };
    };
  };
}
