let
  flk = builtins.getFlake (builtins.toString ./../..);
  lib = flk.inputs.nixpkgs.lib;
  dir = import ./../../lib/dir.nix lib;
in
  lib.runTests {
    testDirToAttrSetOnlyFiles = {
      expr = dir.dirToAttrSet ../fixtures/trivial { handleDir = false; };
      expected = {
        one = 1;
        two = 2;
        three = 3;
      };
    };

    testDirToAttrSetAll = {
      expr = dir.dirToAttrSet ../fixtures/trivial {};
      expected = {
        one = 1;
        two = 2;
        three = 3;
        internal = 4;
      };
    };
  }
