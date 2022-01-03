{stdenv, fetchurl }:

let
  version = "8178a66ac2";

in

stdenv.mkDerivation {
  name = "docc";
  src = fetchurl {
    url    = http://artifacts.internal.digitalocean.com/delivery/docc + ("/${version}/docc-${version}-linux-amd64.tar.gz");
    sha256 = "0y2aygfw2s8ycc3zy481pc2fb00fv14a8mmcgn64kyljhddy6m6p";
  };

  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase"];

  installPhase = ''
    mkdir -p $out/bin
    cp docc $out/bin/docc.wrapped
    cp docc.bash $out/bin
    echo $(< $NIX_CC/nix-support/dynamic-linker) $out/bin/docc.wrapped \"\$@\" > $out/bin/docc
    chmod +x $out/bin/docc
  '';
    # patchelf --set-interpreter ${stdenv.glibc}/lib/ld-linux-x86-64.so.2 $out/bin/docc
    # patchelf --set-rpath ${stdenv.glibc}/lib $out/bin/docc
}
