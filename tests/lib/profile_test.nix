let
  rootPath = ./../..;
  fixturePath = ../fixtures;

  flk = builtins.getFlake (builtins.toString rootPath);
  lib = flk.inputs.nixpkgs.lib;

  profile = import ./../../lib/profile.nix flk.inputs;
in
  lib.runTests {
    testLoadProfilesBasic = {
      expr =
        let
          profiles =
            profile.loadProfiles
              (fixturePath + "/profiles")
              { profiles = [ "git" ]; };

          gitProfile =
            builtins.head profiles;

        in
          gitProfile { pkgs = {}; };

      expected = {
        programs.git.enable = true;
      };
    };

  }
