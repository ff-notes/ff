{ pkgs ? (import <nixpkgs> {}) }:
let ff-pkg = { buildStackProject, fetchgit, gmp, zlib, qt5, wrapGAppsHook }:
buildStackProject {
  name = "ff";

  buildInputs = [ gmp zlib qt5.qtbase wrapGAppsHook ];

  shellHook = "unset STACK_IN_NIX_SHELL";
};
  drv = pkgs.haskellPackages.callPackage ff-pkg { buildStackProject = pkgs.haskell.lib.buildStackProject; };
in drv
