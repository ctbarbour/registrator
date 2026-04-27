{
  description = "Registrator development environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      forAllSystems = nixpkgs.lib.genAttrs [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
    in {
      devShells = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          beamPkgs = pkgs.beam.packages.erlang_28;
        in {
          default = pkgs.mkShell {
            packages = [
              beamPkgs.erlang
              beamPkgs.rebar3
              pkgs.just
              pkgs.go-containerregistry
              pkgs.docker-credential-gcr
              pkgs.sops
              pkgs.age
            ];
          };
        });
    };
}
