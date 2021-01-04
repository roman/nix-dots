{

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    homeManager.url = "github:nix-community/home-manager";
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
        homeManager.lib.homeManagerConfiguration {
          inherit username;
          system = "x86_64-linux";
          homeDirectory = "/home/${username}";
          configuration = args: {
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
      };
}
