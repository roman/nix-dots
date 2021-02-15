# git installs git version control system and adds dotfiles configurations
flakeInputs:
{ pkgs, ... }:

{
  home.packages = with pkgs; [
    git
    gitAndTools.hub
    gitAndTools.git-ignore
  ];

  programs.git = {
    enable = true;
    userName = "Roman Gonzalez";
    userEmail = "open-source@roman-gonzalez.info";
    aliases = {
      b = "branch -v";
      d = "diff";
      dc = "diff --cached";
      cam = "commit --amend --no-edit";
      lg = "log --graph --abbrev-commit --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'";
      r = "remote -v";
      st = "status";
      co = "checkout";
      ci = "commit";
    };

    extraConfig = {
      push = {
        default = "current";
        # followTags = "true";
      };
      pull = {
        default = "current";
        rebase = "true";
      };
      # rebase = { autosquash = "true"; };
    };

    ignores = [ ".direnv/" ];
  };
}
