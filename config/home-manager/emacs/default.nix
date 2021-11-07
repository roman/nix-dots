{ homeManager, ... }:
{ pkgs, lib, config, ... }:

{
  # install emacs program
  programs.emacs = {
    enable = true;
    extraPackages =
      (epkgs: with epkgs.melpaPackages; [ vterm lsp-mode lsp-ui dap-mode ]);
    package = if (!config.xsession.enable && !pkgs.stdenv.isDarwin) then
      pkgs.emacs-nox
    else
      pkgs.emacs27;
  };

  # run emacs as a server
  # services.emacs.enable = true;

  # ensure all commands that need an editor use the emacs server
  home.sessionVariables = { EDITOR = "emacsclient"; };

  # initialization of spacemacs on install rather than on first execution
  home.activation.emacs =
    homeManager.lib.hm.dag.entryAfter [ "installPackages" ] ''
      echo "Configuring spacemacs"
      ln -sfT ~/Projects/nix-dots/config/home-manager/emacs/spacemacs-private/spacemacs ~/.spacemacs
      ln -sfT ~/Projects/nix-dots/vendor/spacemacs ~/.spacemacs.d
      rm -rf ~/.spacemacs.d/private
      ln -sfT ~/Projects/nix-dots/config/home-manager/emacs/spacemacs-private ~/.spacemacs.d/private
      emacs --batch -l ~/.emacs.d/init.el --eval "(message \"init\")"
    '';
}
# // lib.optionalAttrs pkgs.stdenv.isLinux {
#   # run emacs as a server
#   services.emacs.enable = true;
# }
