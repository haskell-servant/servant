let nixpkgsSnapshot =
  builtins.fromJSON (builtins.readFile ./nixpkgs.json); in
import (builtins.fetchTarball 
  { url = "https://github.com/NixOS/nixpkgs/tarball/${nixpkgsSnapshot.rev}";
    sha256 = nixpkgsSnapshot.sha256;
  })
  {}
