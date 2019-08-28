{ pkgs ? (import <nixpkgs> {}) }:
let ff-pkg = { buildStackProject, fetchgit, gmp, zlib, qt5, wrapGAppsHook }:
buildStackProject {
  name = "ff";

  src = fetchgit {
    url = "https://github.com/ff-notes/ff";
    rev = "18492f6067720977c25cc012df88fdeb9ffee5cc";
    sha256 = "1c0zd97bl8i7795n8vhfgbg6y7dgqkby279mngfadg2ich28axrr";
  };

  buildInputs = [ gmp zlib qt5.qtbase wrapGAppsHook ];

  shellHook = "unset STACK_IN_NIX_SHELL";
};
  drv = pkgs.haskellPackages.callPackage ff-pkg { buildStackProject = pkgs.haskell.lib.buildStackProject; };
in drv
