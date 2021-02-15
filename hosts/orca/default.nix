{ config, pkgs, ... }:

{

  # imports =
  #   [ # Include my default home packages
  #     ../../home
  #   ];

  home.packages = with pkgs; [
    gcc
    docker-compose
    plantuml
    expect

    # XMonad UI madness
    haskellPackages.xmobar # Information bar for xmonad
    mpg123                 # mp3 sounds from terminal
    feh                    # background
    xorg.xkbcomp           # Custom keybinding maps
    xorg.xmodmap           # Display X keybinding info
    xorg.xbacklight        # Brightness info
    xorg.xev               # Display written keys
    xkbd                   # Digital Keyboard to check keybindings
    xdotool                # Allows manipulation of windows from terminal
    xclip                  # Allows manipulation of clipboard from terminal
    rofi                   # Application launcher
    dmenu                  # Application launcher
    konsole                # Terminal emulator
    xcompmgr               # Allows to make windows transparent
    dunst                  # Notifications
    xorg.xmessage
    # Workarounds

    # Address bug on Locales using nixpkgs
    # https://github.com/NixOS/nixpkgs/issues/8398#issuecomment-186832814
    glibcLocales

    # Fonts
    # corefonts
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
    # noto-fonts-cjk
    # noto-fonts-emoji
    # jetbrains-mono
    # roboto
    roboto-mono

  ];

  xsession.windowManager.xmonad.enable = true;
  xsession.windowManager.xmonad.enableContribAndExtras = true;
  xdg.enable = true;

  fonts.fontconfig.enable = true;
  programs.command-not-found.enable = true;
  programs.autorandr.enable = true;

  services.stalonetray.enable = true;
  services.keybase.enable = true;
}
