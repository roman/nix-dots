{ pkgs, ... }:

let 

  sources = import ../nix/sources.nix;
  overlay-pkgs = import ../pkgs;

in {

  users.users.roman = {
    description = "Roman Gonzalez";
    isNormalUser = true;
    extraGroups = [ "users" "wheel" "docker" "kvm" ];
    home = "/home/roman";
    createHome = true;
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEA5ulJ5RS0bMMOU12XT2xjTkyysAA35cecJl8Q/l7guIIHZQ0OXwXKIiOXzpCYC5m86Ph4VDs1G9+gicYUF43lzY5hxNowPqPH8nbNUYGPR3yKww0YCy8idu4QsI63B1slpWSsbqQJU0Dpy1dQpJUcE1x0+sKJkpRIQ4zdEF1+e7eAx2QFPz/c4n1CJyg34jmYySu4fz4xQHzqqRH5NfADQu0weIwGf7RiJGuDn4X4x1aa+KaMJ2IJDZ+FCTbRSx2KnwqlC8Gx/CeE8eXv3V044wzPOAt/+0kMYh1CzM/JYGftZlTdwGEqWaVTMy+be8igAMX/tcdXQP0Sy/VhPT2tWw=="
    ];
    useDefaultShell = true;
    # shell = pkgs.fish;
  };

  home-manager.users.roman = { pkgs, ... }: {
    imports =  [ ../home ];
    nixpkgs = {
      overlays = [ overlay-pkgs ];
      config.packageOverrides = with pkgs; {
        
      };
    };
  };


}
