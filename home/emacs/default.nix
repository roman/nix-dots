{ pkgs, ... }:

{
  programs.emacs = {
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

  home.activation.emacs = pkgs.hm-lib.dag.entryAfter [ "installPackages" ] ''
    ln -sfT /etc/nix/dots/home/emacs/spacemacs-private/spacemacs ~/.spacemacs
    ln -sfT /etc/nix/dots/home/emacs/spacemacs ~/.emacs.d
    rm -rf ~/.emacs.d/private
    ln -sfT /etc/nix/dots/home/emacs/spacemacs-private ~/.emacs.d/private
    emacs --batch -l ~/.emacs.d/init.el --eval "(message \"init\")"
  '';
}
