{ homeManager, ... }:

let

  buildHomeManagerConfig = { modules, overlays, ... }: {
    home.stateVersion = "20.09";
    nixpkgs = {
      overlays = overlays;
      config = { allowUnfree = true; };
    };
    imports = modules;
  };

in

{

  # buildVagrantGuestOS is a function that creates a configuration that is vagrant
  # guest friendly (e.g. ubuntu machine in vagrant)
  buildVagrantGuestOS = { username, modules, overlays, ... }:
    homeManager.lib.homeManagerConfiguration {
      inherit username;
      system = "x86_64-linux";
      configuration = buildHomeManagerConfig { inherit modules overlays; };
    };

  inherit buildHomeManagerConfig;

}
