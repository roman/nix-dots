{ modulesPath, lib, ... }:
let
  do-userdata     = lib.optional (builtins.pathExists ./do-userdata.nix) ./do-userdata.nix;
  do-nixos-module = modulesPath + "/virtualisation/digital-ocean-config.nix";
  dots-system   = ../../system;
in
  {
    imports =  do-userdata ++ [do-nixos-module dots-system];
    security.sudo.extraConfig =
    ''
      Defaults:root,%wheel env_keep+=LOCALE_ARCHIVE
      Defaults:root,%wheel env_keep+=NIX_PATH
      Defaults:root,%wheel env_keep+=TERMINFO_DIRS
      Defaults env_keep+=SSH_AUTH_SOCK
      Defaults lecture = never
      root   ALL=(ALL) SETENV: ALL
      %wheel ALL=(ALL) NOPASSWD: ALL, SETENV: ALL
    '';
  }
