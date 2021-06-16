pkgs:
  let
    nodeEnv = pkgs.callPackage ./node-env.nix {};
    keytar-project = pkgs.callPackage ./node-packages.nix {
      inherit nodeEnv;
    };
    args = keytar-project.args // {
      src = pkgs.fetchFromGitHub {
        owner = "emacs-grammarly";
        repo = "keytar-cli";
        rev = "0.1.4";
        sha256 = "sha256-K2xmCCu6j56I/sub5r7SxX7wI2mdt3JFnFiisu9ztLM=";
      };
      buildInputs = builtins.attrValues {
        inherit (pkgs) pkg-config libsecret;
      };
    };
  in
    # use generated nix package from the original project package-lock.json
    nodeEnv.buildNodePackage args
