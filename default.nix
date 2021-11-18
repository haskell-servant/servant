with (builtins.fromJSON (builtins.readFile ./nix/nixpkgs.json));
{
  pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  }) {}
, compiler ? "ghc883"
}:
let
  overrides = self: super: {
    servant              = self.callCabal2nix "servant"              ./servant              {};
    servant-docs         = self.callCabal2nix "servant-docs"         ./servant-docs         {};
    servant-pipes        = self.callCabal2nix "servant-pipes"        ./servant-pipes        {};
    servant-server       = self.callCabal2nix "servant-server"       ./servant-server       {};
    servant-client       = self.callCabal2nix "servant-client"       ./servant-client       {};
    servant-foreign      = self.callCabal2nix "servant-foreign"      ./servant-foreign      {};
    servant-conduit      = self.callCabal2nix "servant-conduit"      ./servant-conduit      {};
    servant-machines     = self.callCabal2nix "servant-machines"     ./servant-machines     {};
    servant-client-core  = self.callCabal2nix "servant-client-core"  ./servant-client-core  {};
    servant-http-streams = self.callCabal2nix "servant-http-streams" ./servant-http-streams {};
  };
  hPkgs = pkgs.haskell.packages.${compiler}.override { inherit overrides; };
in
  with hPkgs;
  {
    inherit
      servant
      servant-client
      servant-client-core
      servant-conduit
      servant-docs
      servant-foreign
      servant-http-streams
      servant-machines
      servant-pipes
      servant-server;
  }

