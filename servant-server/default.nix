{ pkgs ? import <nixpkgs> { config.allowUnfree = true; }
, src ?  builtins.filterSource (path: type:
    type != "unknown" &&
    baseNameOf path != ".git" &&
    baseNameOf path != "result" &&
    baseNameOf path != "dist") ./.
, servant ? import ../servant {}
}:
pkgs.haskellPackages.buildLocalCabalWithArgs {
  name = "servant-server";
  inherit src;
  args = {
      inherit servant;
  };
}
