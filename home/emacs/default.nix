flakeInputs:
{ pkgs, lib, ... }:

{

  programs.emacs = builtins.trace (builtins.attraNems flakeInputs) {
    enable = true;
    package = pkgs.zoo-emacs;
    extraPackages = (epkgs:
      [ epkgs.melpaPackages.vterm
        epkgs.melpaPackages.lsp-mode
        epkgs.melpaPackages.lsp-ui
        epkgs.melpaPackages.dap-mode
      ]
    );
  };

  ## home.activation.emacs = homeManager.lib.dag.entryAfter [ "installPackages" ] ''
  home.activation.emacs = builtins.trace (builtins.attrNames lib) flakeInputs.homeManager.lib.dag.entryAfter [ "installPackages" ] ''
    ln -sfT /etc/nix/dots/home/emacs/spacemacs-private/spacemacs ~/.spacemacs
    ln -sfT /etc/nix/dots/home/emacs/spacemacs ~/.emacs.d
    rm -rf ~/.emacs.d/private
    ln -sfT /etc/nix/dots/home/emacs/spacemacs-private ~/.emacs.d/private
    emacs --batch -l ~/.emacs.d/init.el --eval "(message \"init\")"
  '';
}
