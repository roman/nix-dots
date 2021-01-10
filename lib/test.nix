let
  flk = builtins.getFlake (builtins.toString ./..);
  lib = flk.inputs.nixpkgs.lib;
in
  lib.runTests {
    testFoo = {
      expr = "foo";
      expected = "foo";
    };
  }
