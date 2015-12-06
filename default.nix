{ haskellngPackages ? (import <nixpkgs> {}).haskellngPackages,
  pkgs ? (import <nixpkgs> {}).pkgs
}:
let
  env = haskellngPackages.ghcWithPackages (p: with p; [
    binary byteorder bytestring containers exceptions free lens mmorph mtl
    PhiledCommon pipes pipes-binary pipes-bytestring pipes-safe QuickCheck sdl2
    sdl2-ttf transformers unix
    cabal-install hlint
  ]);
in pkgs.stdenv.mkDerivation {
  name = "GUIS";
  buildInputs = [ env pkgs.dejavu_fonts ];
  fonts = pkgs.dejavu_fonts;
}
