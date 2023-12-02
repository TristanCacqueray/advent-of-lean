{
  description = "Advent of Lean";

  inputs.nixpkgs.url =
    "github:NixOS/nixpkgs/3176a8460ff51d1fa223e2962b11bd85543a36ba";

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
      lean = pkgs.stdenv.mkDerivation rec {
        pname = "lean";
        version = "4.3.0";
        src = pkgs.fetchurl {
          url =
            "https://github.com/leanprover/lean4/releases/download/v${version}/lean-${version}-linux.tar.zst";
          sha256 = "sha256-m59NJ+GiuRz7IWvKzeUAG4/eAb+QqQcmSypNSLOWdhI=";
        };
        buildInputs = [ pkgs.zstd ];
        unpackPhase = ''
          # Unpack sources
          tar xf $src
          mv lean-* $out
        '';
        dontStrip = true;
        dontInstall = true;
      };
    in { devShell."x86_64-linux" = pkgs.mkShell { buildInputs = [ lean pkgs.elan ]; }; };
}
