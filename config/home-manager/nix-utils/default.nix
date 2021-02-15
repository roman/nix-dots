# nix-utils installs binaries that make development of nix easier
flakeInputs:
{ pkgs, lib, ... }:

with pkgs;

{
  home.packages = [
    # automatic formatting of nix files
    nixfmt

    # utlities to get SHAs for depedencies
    nix-prefetch-git
    nix-prefetch-github
    nix-tree

    # utlity to add cache servers
    cachix
  ];
}
