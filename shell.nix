{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = [
    ( pkgs.haskellPackages.ghcWithPackages (pkgs: [
        pkgs.split
        pkgs.process
    ]) )
  ];
}
