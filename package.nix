let pkgs = import <nixpkgs> {};
in pkgs.stdenv.mkDerivation {
  name = "ush";
  src = ./.;
  buildInputs = [
    ( pkgs.haskellPackages.ghcWithPackages (pkgs: [
        pkgs.split
        pkgs.process
    ]) )
  ];
  builder = ./pkg_builder.sh;
}
