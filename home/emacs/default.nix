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
      (with epkgs.melpaPackages; [
        ace-jump-helm-line
        ace-link
        ac-ispell
        afternoon-theme
        alect-themes
        alert
        ample-theme
        ample-zen-theme
        anti-zenburn-theme
        anzu
        apropospriate-theme
        async
        auto-compile
        auto-complete
        auto-dictionary
        auto-highlight-symbol
        autothemer
        auto-yasnippet
        avy
        badwolf-theme
        bind-key
        bind-map
        birds-of-paradise-plus-theme
        bubbleberry-theme
        bundler
        busybee-theme
        cargo
        cherry-blossom-theme
        chruby
        clean-aindent-mode
        clues-theme
        cmm-mode
        coffee-mode
        color-identifiers-mode
        color-theme-sanityinc-solarized
        color-theme-sanityinc-tomorrow
        column-enforce-mode
        company
        company-cabal
        company-ghc
        company-ghci
        company-go
        company-nixos-options
        company-shell
        company-statistics
        company-web
        cyberpunk-theme
        dakrone-theme
        darkburn-theme
        darkmine-theme
        darkokai-theme
        darktooth-theme
        dash
        define-word
        diff-hl
        diminish
        direnv
        django-theme
        dockerfile-mode
        doom-themes
        dracula-theme
        dumb-jump
        elisp-slime-nav
        emmet-mode
        epl
        espresso-theme
        eval-sexp-fu
        evil
        evil-anzu
        evil-args
        evil-ediff
        evil-exchange
        evil-iedit-state
        evil-indent-plus
        evil-lisp-state
        evil-magit
        evil-matchit
        evil-mc
        evil-nerd-commenter
        evil-numbers
        evil-search-highlight-persist
        evil-surround
        evil-tutor
        evil-visual-mark-mode
        evil-visualstar
        exec-path-from-shell
        exotica-theme
        expand-region
        eyebrowse
        f
        fancy-battery
        farmhouse-theme
        fill-column-indicator
        fish-mode
        flatland-theme
        flatui-theme
        flx
        flx-ido
        flycheck
        flycheck-haskell
        flycheck-pos-tip
        flycheck-rust
        flyspell-correct
        flyspell-correct-helm
        fringe-helper
        fuzzy
        gandalf-theme
        gh
        ghc
        gh-md
        gist
        gitattributes-mode
        git-commit
        gitconfig-mode
        git-gutter
        git-gutter-fringe
        github-browse-file
        github-clone
        github-search
        gitignore-mode
        git-link
        git-messenger
        git-timemachine
        gntp
        gnuplot
        go-eldoc
        go-guru
        golden-ratio
        go-mode
        google-translate
        gotham-theme
        goto-chg
        grandshell-theme
        groovy-mode
        gruber-darker-theme
        gruvbox-theme
        haml-mode
        haskell-mode
        haskell-snippets
        hc-zenburn-theme
        helm
        helm-ag
        helm-company
        helm-core
        helm-css-scss
        helm-c-yasnippet
        helm-descbinds
        helm-flx
        helm-gitignore
        helm-hoogle
        helm-make
        helm-mode-manager
        helm-nixos-options
        helm-projectile
        helm-swoop
        helm-themes
        hemisu-theme
        heroku-theme
        highlight
        highlight-indentation
        highlight-numbers
        highlight-parentheses
        hlint-refactor
        hl-todo
        ht
        htmlize
        hungry-delete
        hydra
        iedit
        indent-guide
        inf-ruby
        inkpot-theme
        insert-shebang
        intero
        ir-black-theme
        jazz-theme
        jbeans-theme
        js2-mode
        js2-refactor
        js-doc
        json-mode
        json-reformat
        json-snatcher
        key-chord
        light-soap-theme
        link-hint
        linum-relative
        livid-mode
        log4e
        logito
        lorem-ipsum
        lush-theme
        lv
        macrostep
        madhat2r-theme
        magit
        magit-gh-pulls
        magit-gitflow
        magit-popup
        majapahit-theme
        markdown-mode
        markdown-toc
        marshal
        material-theme
        minimal-theme
        minitest
        moe-theme
        molokai-theme
        monochrome-theme
        monokai-theme
        move-text
        multiple-cursors
        multi-term
        mustang-theme
        naquadah-theme
        navorski
        neotree
        nginx-mode
        nix-mode
        nixos-options
        noctilux-theme
        obsidian-theme
        occidental-theme
        oldlace-theme
        omtose-phellack-theme
        open-junk-file
        organic-green-theme
        org-bullets
        org-category-capture
        org-download
        orgit
        org-kanban
        org-mime
        org-pomodoro
        org-present
        org-projectile
        ox-gfm
        packed
        paradox
        paredit
        parent-mode
        pcache
        pcre2el
        persp-mode
        phoenix-dark-mono-theme
        phoenix-dark-pink-theme
        pkg-info
        planet-theme
        plantuml-mode
        popup
        popwin
        pos-tip
        powerline
        professional-theme
        projectile
        protobuf-mode
        pug-mode
        purple-haze-theme
        racer
        railscasts-theme
        rainbow-delimiters
        rainbow-identifiers
        rake
        rbenv
        rebecca-theme
        request
        restart-emacs
        reverse-theme
        robe
        rspec-mode
        rubocop
        ruby-test-mode
        ruby-tools
        rust-mode
        rvm
        s
        sass-mode
        scss-mode
        seti-theme
        simple-httpd
        skewer-mode
        slim-mode
        smartparens
        smeargle
        smyx-theme
        soft-charcoal-theme
        soft-morning-theme
        soft-stone-theme
        solarized-theme
        soothe-theme
        spacegray-theme
        spaceline
        subatomic256-theme
        subatomic-theme
        sublime-themes
        sunny-day-theme
        tagedit
        tango-plus-theme
        tangotango-theme
        tao-theme
        toc-org
        toml-mode
        toxi-theme
        transient
        twilight-anti-bright-theme
        twilight-bright-theme
        twilight-theme
        ujelly-theme
        underwater-theme
        use-package
        uuidgen
        vi-tilde-fringe
        volatile-highlights
        web-beautify
        web-completion-data
        web-mode
        which-key
        white-sand-theme
        winum
        with-editor
        ws-butler
        yaml-mode
        yasnippet
        zen-and-art-theme
        zenburn-theme
      ])
      ++ (with epkgs.elpaPackages; [
        ace-window
        adaptive-wrap
        aggressive-indent
        # archives
        csv-mode
        # evil-unimpaired
        # git-gutter-fringe-plus
        # git-gutter-plus
        mmm-mode
        rainbow-mode
        spinner
        # tango
        undo-tree
      ])
      ++ (with epkgs.orgPackages; [ org org-plus-contrib ]));
    # extraPackages = (epkgs:
    #   (with epkgs.elpaPackages; [ aggressive-indent auctex delight undo-tree ])
    #   ++ (with epkgs.melpaPackages; [
    #     all-the-icons
    #     amx
    #     apropospriate-theme
    #     auctex-latexmk
    #     auto-compile
    #     company
    #     company-auctex
    #     company-math
    #     company-quickhelp
    #     company-reftex
    #     company-shell
    #     company-statistics
    #     counsel
    #     counsel-projectile
    #     deadgrep
    #     diff-hl
    #     direnv
    #     doom-modeline
    #     doom-themes
    #     ebib
    #     evil
    #     # evil-evilified-state
    #     evil-collection
    #     evil-commentary
    #     evil-matchit
    #     evil-goggles
    #     evil-lion
    #     evil-magit
    #     evil-smartparens
    #     evil-surround
    #     exec-path-from-shell
    #     flycheck
    #     flyspell-correct-ivy
    #     general
    #     gitattributes-mode
    #     gitconfig-mode
    #     gitignore-mode
    #     helpful
    #     hl-todo
    #     hydra
    #     ivy
    #     ivy-bibtex
    #     ivy-yasnippet
    #     latex-extra
    #     macrostep
    #     magic-latex-buffer
    #     magit
    #     markdown-mode
    #     nix-mode
    #     no-littering
    #     org-ref
    #     org-super-agenda
    #     outshine
    #     projectile
    #     rainbow-delimiters
    #     smartparens
    #     swiper
    #     use-package
    #     vterm
    #     which-key
    #     ws-butler
    #     yasnippet
    #     yasnippet-snippets

    #     ## python packages
    #     anaconda-mode
    #     live-py-mode
    #     py-isort
    #     pytest

    #   ]) ++ (with epkgs.orgPackages; [ org org-plus-contrib ]));
  };

  home.activation.emacs = execute ''
    ln -sfT /etc/config/home/emacs/spacemacs-private/spacemacs ~/.spacemacs
    ln -sfT /etc/config/home/emacs/spacemacs ~/.emacs.d
    rm -rf ~/.emacs.d/private
    ln -sfT /etc/config/home/emacs/spacemacs-private ~/.emacs.d/private
    emacs --batch -l ~/.emacs.d/init.el --eval "(message \"init\")"
  '';
}
