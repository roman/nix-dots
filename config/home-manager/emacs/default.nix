{ homeManager, ... }:
{ pkgs, lib, config, ... }:
{

  # install emacs program
  programs.emacs = lib.mkMerge [
    {
      package = pkgs.emacs27;
      enable = true;
      extraPackages = (epkgs:
        with epkgs.melpaPackages; [
          vterm
          lsp-mode
          lsp-ui
          dap-mode
        ]
      );
    }
    # if we are not running xsession, install emacs-nox
    (lib.mkIf (!config.xsession.enable) {
      package = pkgs.emacs-nox;
    })
  ];

  # run emacs as a server
  services.emacs.enable = true;

  # ensure all commands that need an editor use the emacs server
  home.sessionVariables = {
    EDITOR = "emacsclient";
  };

  # initialization of spacemacs on install rather than on first execution
  home.activation.emacs = homeManager.lib.hm.dag.entryAfter [ "installPackages" ] ''
    echo "setting up shit around"
    ln -sfT /etc/nix/dots/config/home-manager/emacs/spacemacs-private/spacemacs ~/.spacemacs
    ln -sfT /etc/nix/dots/vendor/spacemacs ~/.emacs.d
    rm -rf ~/.emacs.d/private
    ln -sfT /etc/nix/dots/config/home-manager/emacs/spacemacs-private ~/.emacs.d/private
    emacs --batch -l ~/.emacs.d/init.el --eval "(message \"init\")"
  '';
}
