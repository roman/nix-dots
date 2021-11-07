{ config, pkgs, ... }:

{

  # imports =
  #   [ # Include my default home packages
  #     ../../home
  #   ];

  home.packages = with pkgs; [
    docker-compose
    plantuml
    expect

    # Fonts
    fontconfig
    dejavu_fonts
    fira-code
    fira-code-symbols
    font-awesome-ttf
    hasklig
    inconsolata
    ubuntu_font_family
    source-code-pro
    terminus_font

    # liberation_ttf
    noto-fonts
    roboto-mono
  ];

  fonts.fontconfig.enable = true;
  programs.command-not-found.enable = true;
}
