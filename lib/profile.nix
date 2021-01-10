flakeInputs:

let
  lib = flakeInputs.nixpkgs.lib;

  loadProfile = profilesDir: acc: profileName:
    let
      contents = import (profilesDir + "/${profileName}.nix") flakeInputs;
    in
      acc ++ [contents];
in
{

  loadProfiles = profilesDir: userSettings:
    if lib.hasAttr "profiles" userSettings then
      lib.foldl' (loadProfile profilesDir) [] (lib.getAttr "profiles" userSettings)
    else
      [];
}
