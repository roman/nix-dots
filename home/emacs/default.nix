{ config, pkgs, lib, ... }:

with import ../../lib;

let
  sources = import ../../nix/sources.nix;
  zoo = pkgs.emacs-nox;
in {
  nixpkgs.overlays = [ (import sources.emacs-overlay) ];
  programs.emacs = {
    enable = true;
    package = zoo;
    extraPackages = (epkgs:
      (with epkgs.elpaPackages; [ aggressive-indent auctex delight undo-tree ])
      ++ (with epkgs.melpaPackages; [
        all-the-icons
        amx
        apropospriate-theme
        auctex-latexmk
        auto-compile
        company
        company-auctex
        company-math
        company-quickhelp
        company-reftex
        company-shell
        company-statistics
        counsel
        counsel-projectile
        deadgrep
        diff-hl
        direnv
        doom-modeline
        doom-themes
        ebib
        evil
        evil-collection
        evil-commentary
        evil-goggles
        evil-lion
        evil-magit
        evil-smartparens
        evil-surround
        exec-path-from-shell
        flycheck
        flyspell-correct-ivy
        general
        gitattributes-mode
        gitconfig-mode
        gitignore-mode
        helpful
        hl-todo
        hydra
        ivy
        ivy-bibtex
        ivy-yasnippet
        latex-extra
        macrostep
        magic-latex-buffer
        magit
        markdown-mode
        navorski
        no-littering
        org-ref
        org-super-agenda
        outshine
        projectile
        rainbow-delimiters
        smartparens
        swiper
        use-package
        which-key
        ws-butler
        yasnippet
        yasnippet-snippets
      ]) ++ (with epkgs.orgPackages; [ org org-plus-contrib ]));
  };

  home.activation.emacs = execute ''
    ln -sfT /etc/config/home/emacs/ ~/.emacs.d
    emacs --batch -l ~/.emacs.d/init.el --eval "(zoo--byte-compile-zoo)"
  '';
}

