# bash configures bash related dotfiles and installs tooling that is normally
# used in bash scripts
flakeInputs:
{ pkgs, lib, ... }:

with pkgs;

{

  programs = {
    zoxide.enable = true;
    man.enable = true;

    bat = {
      enable = true;
      config = {
        pager = "less -FR";
        style = "numbers,changes";
      };
    };

    fzf = {
      enable = true;
      changeDirWidgetCommand = "fd --type d";
      defaultCommand = "fd --type f";
    };

    starship = {
      enable = true;
      enableBashIntegration = true;
      settings = {
        git_commit.only_detached = false;
        git_commit.commit_hash_length = 8;
      };
    };

    dircolors.enable = true;

    direnv = {
      enable = true;
      enableBashIntegration = true;
    };

    direnv.nix-direnv = {
      enable = true;
      enableFlakes = true;
    };

    bash = {
      enable = true;
      shellAliases = {
        fzp  = ''fzf --preview "bat --color=always {}"'';
        cat  = "bat";
        ll   = "exa -lagFT --git-ignore --git --level 1";
        ls = "exa -a --git-ignore";
        tree = "exa --tree --git-ignore";
      };
      bashrcExtra = lib.mkAfter (builtins.readFile ./bashrc);
      profileExtra = lib.mkAfter (builtins.readFile ./profile);
    };
  };

  home.packages = [
    # fancy banners
    figlet

    # dictionary support
    (aspellWithDicts (ps: with ps; [ en es ]))

    # sqlite
    sqlite
    sqlite-interactive

    # default editor support
    vim

    # bash scripting with json
    jq

    # fancy commands
    exa
    fd
    # ripgrep
    htop
    bottom

    # http requests from bash
    wget
    curl
  ];
}
