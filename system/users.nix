{ pkgs, ... }:

let 

  sources = import ../nix/sources.nix;
  overlay-pkgs = import ../pkgs;

in {

  users.users.roman = {
    description = "Roman Gonzalez";
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" "kvm" ];
    shell = pkgs.fish;
  };

  home-manager.users.sondre = { pkgs, ... }: {
    imports =  [ ../home ];
    nixpkgs = {
      overlay = [ overlay-pkgs ];
      config.packageOverrides = pkgs; {
        
      };
    };
  };

}
