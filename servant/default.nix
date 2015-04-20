{ pkgs ? import <nixpkgs> { config.allowUnfree = true; }
, src ?  builtins.filterSource (path: type:
    type != "unknown" &&
    baseNameOf path != ".git" &&
    baseNameOf path != "result" &&
    baseNameOf path != "dist") ./.
}:
pkgs.haskellPackages.buildLocalCabalWithArgs {
  name = "servant";
  inherit src;
  args = {};
}
