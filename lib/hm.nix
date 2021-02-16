{

  buildHomeManagerConfig = { modules, overlays, ... }: {
    home.stateVersion = "20.09";
    nixpkgs = {
      overlays = overlays;
      config = { allowUnfree = true; };
    };
    imports = modules;
  };

}
