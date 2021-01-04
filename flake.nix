{

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    homeManager.url = "github:nix-community/home-manager/release-20.09";
    homeManager.inputs.nixpkgs.follows = "nixpkgs";

    emacs.url = "github:nix-community/emacs-overlay";
  };


  outputs = { self, nixpkgs, homeManager, emacs }:
    let
      # we use the home manager DAG API to inject scripts after installation
      hm-lib = super: self:
        {
          hm-lib = homeManager.lib.hm;
        };

      # we use general purpose package names and alter them between non-ui and ui
      # setups (same recipe, different behavior)
      guest-zoo-packages = super: self:
        {
          zoo-emacs = self.emacs-nox;
        };

      # buildVagrantGuest is a function that creates a configuration that is
      # vagrant guest friendly (e.g. ubuntu machine in vagrant)
      buildVagrantGuest = username:
        homeManager.lib.homeManagerConfiguration (buildHomeManagerPlainConfig username);

      buildHomeManagerPlainConfig = username: {
        inherit username;
        system = "x86_64-linux";
        homeDirectory = "/home/${username}";
        configuration = args: {
          home.stateVersion = "20.09";
          imports = [ ./home ];
          nixpkgs = {
            overlays = [ emacs.overlay hm-lib guest-zoo-packages ];
            config = { allowUnfree = true; };
          };
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
                homeManager.nixosModules.home-manager
                {
                  home-manager.users.vagrant = {
                    home.stateVersion = "20.09";
                    nixpkgs = {
                      overlays = [ emacs.overlay hm-lib guest-zoo-packages ];
                      config = { allowUnfree = true; };
                    };
                    imports = [ ./home ];
                  };
                }
              ];
          };
        };

      };
}
