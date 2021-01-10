lib:

{

  # dirToAttrSet creates an attribute set with the contents of nix files inside
  # a given directory. Each key in the attribute set is the file name with the
  # `.nix` suffix stripped, and each value is the nix expression returned by the
  # import call.
  dirToAttrSet = dirPath: { handleDir ? true }:
    let
      isDefault = name: (name == "default.nix");

      isNixFile =
        lib.hasSuffix ".nix";

      nixFilesOnly =
        fileName: fileType:
          (fileType == "regular" &&
           !(isDefault fileName) &&
           isNixFile fileName) ||
            (fileType == "directory" && handleDir);

      filterNixFiles =
        lib.filterAttrs nixFilesOnly;

      buildEntry =
        acc: fileName:
          if fileName == "" then
            acc
          else
            let
              path = dirPath + "/${fileName}";
              expr = import path;
            in
              lib.setAttr acc (lib.removeSuffix ".nix" fileName) expr;

      buildAttrSet =
        lib.foldl' buildEntry {};

    in
      lib.pipe dirPath
        [
          builtins.readDir
          filterNixFiles
          lib.attrNames
          buildAttrSet
        ];
}
