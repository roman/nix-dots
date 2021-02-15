{

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    homeManager.url = "github:nix-community/home-manager/release-20.09";
    homeManager.inputs.nixpkgs.follows = "nixpkgs";

    emacs.url = "github:nix-community/emacs-overlay";
  };


  outputs = inputs@{ self, nixpkgs, homeManager, emacs }:
    let
      # buildVagrantGuest is a function that creates a configuration that is
      # vagrant guest friendly (e.g. ubuntu machine in vagrant)
      buildVagrantGuest = username:
        homeManager.lib.homeManagerConfiguration (buildHomeManagerPlainConfig username);

      homeModules = import ./config/home-manager inputs;

      buildHomeManagerPlainConfig = username: {
        inherit username;
        system = "x86_64-linux";
        homeDirectory = "/home/${username}";
        configuration = args: {
          home.stateVersion = "20.09";
          nixpkgs = {
            overlays = [ emacs.overlay ];
            config = { allowUnfree = true; };
          };
          imports = with homeModules; [
            bash
            docker
            homeModules.emacs
            git
            nix-utils
          ];
        };
      };

    in
      {

        homeManagerConfigurations = {
          # normally ubuntu and vagrant are names found in vagrant images
          ubuntu-guest = buildVagrantGuest "ubuntu";
          vagrant-guest = buildVagrantGuest "vagrant";
        };

        nixosConfigurations = {
          nixbox = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules =
              [
                ./hosts/nixbox
                ./config/os/docker
                homeManager.nixosModules.home-manager
                {
                  home-manager.users.vagrant = {
                    home.stateVersion = "20.09";
                    nixpkgs = {
                      overlays = [ emacs.overlay ];
                      config = { allowUnfree = true; };
                    };
                    imports = with homeModules; [
                      homeModules.emacs
                      bash
                      git
                      nix-utils
                    ];
                  };
                }
              ];
          };
        };
      };
}
