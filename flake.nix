{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default";
  };
  outputs =
    { nixpkgs, systems, ... }:
    let
      eachSystem =
        f: nixpkgs.lib.genAttrs (import systems) (system: f (import nixpkgs { inherit system; }));
    in
    {
      devShells = eachSystem (pkgs: {
        default = pkgs.mkShell {
          buildInputs = with pkgs; [
            nyxt
            (pkgs.sbcl.buildASDFSystem {
              pname = "nx-router";
              version = "0.3.0";
              src = ./.;
              lispLibs = [ nyxt ];
            })
          ];
        };
      });
    };
}
