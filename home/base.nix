{ pkgs, lib, ... }:

with pkgs;

{

  programs = {

    # home-manager.enable = true;
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

    fish.shellAliases = {
      fzp = ''fzf --preview "bat --color=always {}"'';
      l = "exa -lagFT --git-ignore --git --level 1";
      ls = "exa -a --git-ignore";
      tree = "exa --tree --git-ignore";
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
      enableNixDirenvIntegration = true;
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
    (aspellWithDicts (ps: with ps; [ en es ]))

    figlet

    sqlite
    sqlite-interactive

    git
    gitAndTools.hub

    vim
    jq

    exa
    fd
    ripgrep

    htop
    ytop

    wget
    curl

    cachix

    nixfmt
    nix-prefetch-git
    nix-prefetch-github

    zlib
  ];
}
