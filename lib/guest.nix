{ homeManager, ... }@flake-inputs:

let
  lib = flake-inputs.nixpkgs.lib;
  fs = import fs.nix { inherit lib; };

  homeManagerConfigType = "homeManagerConfig";
  nixosConfigType = "nixosConfig";

  loadProfile =
    profileName:
      import (../profiles) + "/${profileName}";

  profilesToHomeManagerImports =
    { profiles, ... }:
      lib.map loadProfile profiles;

  buildUsersConfig =
    path:
    { overlays ? [], ... }:
    let
      usersDir =
        path + "/users";

      fooxyz =
        fs.dirToAttrSet usersDir { handleDir = false; };

      # fetchUserConfig =
      #   _username: config:
      #   let
      #     hmImports =
      #       profilesToHomeManagerImports config;

      #     nixosConfig =
      #       map.filterAttrs
      #         (name: _: name != "profiles" && name != "overlays")
      #         config;

      #     hmConfig =
      #       {
      #         home.stateVersion = "20.09";
      #         nixpkgs = {
      #           overlays = overlays;
      #           config = { allowUnfree = true; };
      #         };
      #         imports = hmImports;
      #       };
      #   in
      #     builtins.trace "wtf" {
      #       homeManager = hmConfig;
      #       nixos = nixosConfig;
      #     };
    in
      lib.attrNames fooxyz;
      # lib.mapAttrs fetchUserConfig userConfigs;

  build = {

    guestOS =
      { overlays ? [], system ? "x86_64-Linux", ... }@args:
      path:
      hostName:
      let
        hmConfig =
          lib.mapAttrs
            (_: config: config.homeManager)
            (buildUsersConfig path args);
      in
        {
          homeManagerConfiguration =
            lib.nameValuePair
              hostName
              (homeManager.lib.homeManagerConfiguration hmConfig);
        };

    vagrantNixOS =
      { overlays ? [], modules ? [], system ? "x86_64-Linux", ... }@args:
      path:
      hostName:
      let
        configurationModule =
          import ./config-templates/vagrant.nix { inherit hostName; };

        nixFlakeModule =
          { pkgs, ... }:
          {
            nix = {
              package = pkgs.nixFlakes;
              extraOptions = ''
                  experimental-features = nix-command flakes
                '';
            };
          };

        usersConfig =
          buildUsersConfig path args;

        nixosConfig =
          {
            inherit system;
            modules = [
              # always include home-manager
              homeManager.nixosModules.home-manager
              # always enable nix flakes
              nixFlakeModule
              # base configuration module for vagrant machines
              configurationModule
              # deal with the profile configuration for each user
              (_: {
                home-manager.users =
                  lib.mapAttrs (_:config: config.homeManager) usersConfig;
              })
              # users OS configuration
              (_: {
                users.users =
                  lib.mapAttrs (_:config: config.nixos) usersConfig;
              })
            ] ++ modules;
          };

      in
        {
          nixosConfigurations =
            lib.nameValuePair hostName nixosConfig;
        };

    # digitalOceanNixOS =
    #   { path, modules ? [], system ? "x86_64-Linux", ... }:
    #   path:
    #   hostName:
    #     let
    #       configurationModule =
    #         import ./config-templates/digitalocean.nix { inherit hostName; };
    #       usersDir =
    #         path + "/users";
    #       configFetcherPerUser =
    #         fs.dirToAttrSet usersDir;
    #     in
    #       {};
  };
in
{

  inherit buildUsersConfig;

  # processHosts is called by flake.nix to get the configuration
  # from all hosts files
  processHosts =
    path:
    let
      hosts =
        fs.dirToAttrSet path;

      processHost =
        acc: hostName:
        let
          getHostConfig = hosts.${hostName} build;
        in
          acc + [getHostConfig (path + "/${hostName}") hostName];

      # rawHosts0 ::
      #  [{ nixosConfigurations = [{name = host; value = nixosConfig; }, ... ];
      #     homeManagerConfigurations = [{name = host; value = hmConfig; }, ...];
      #   }, ... ];
      #
      rawHosts0 =
        lib.foldl' processHost [] hosts;

      # rawHosts1 ::
      #  { nixosConfigurations = [{name = host; value = nixosConfig; }, ... ];
      #    homeManagerConfigurations = [{name = host; value = hmConfig; }, ...];
      #  };
      #
      # All values are merged
      rawHosts1 =
        lib.foldAttrs (a: b: [a] ++ b) [] rawHosts0;
    in
      lib.mapAttrs (_:v: lib.listToAttrs v) rawHosts1;

  inherit build;
}
