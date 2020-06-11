{ pkgs, ... }:

{
  home.packages = with pkgs; [
    gitAndTools.git-ignore
    gitAndTools.hub
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
      lg =
        "log --graph --abbrev-commit --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'";
      r = "remote -v";
      s = "status";
    };

    # extraConfig = {
    #   push = {
    #     default = "current";
    #     followTags = "true";
    #   };
    #   pull = {
    #     default = "current";
    #     rebase = "true";
    #   };
    #   rebase = { autosquash = "true"; };
    # };
    # ignores = [ ".envrc" ".direnv/" ];
  };
}

