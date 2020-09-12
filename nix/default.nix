{ sources ? import ./sources.nix, ... }:

with {
  overlay = _: pkgs: {
    niv = import sources.niv { };
    home-manager = (import sources.home-manager { }).home-manager;
  };
};

import sources.nixpkgs {
  overlays = [ overlay ];
  config = { };
}

