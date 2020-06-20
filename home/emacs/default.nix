{ config, pkgs, lib, ... }:

with import ../../lib;

let
  sources = import ../../nix/sources.nix;
in {
  nixpkgs.overlays = [ (import sources.emacs-overlay) ];
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-nox;
    extraPackages = (epkgs:
      [ epkgs.melpaPackages.vterm ]
    );
  };

  home.activation.emacs = execute ''
    ln -sfT /etc/config/home/emacs/spacemacs-private/spacemacs ~/.spacemacs
    ln -sfT /etc/config/home/emacs/spacemacs ~/.emacs.d
    rm -rf ~/.emacs.d/private
    ln -sfT /etc/config/home/emacs/spacemacs-private ~/.emacs.d/private
    emacs --batch -l ~/.emacs.d/init.el --eval "(message \"init\")"
  '';
}
