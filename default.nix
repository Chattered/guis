{ pkgs ? (import <nixpkgs> {}).pkgs
}:
let
  myemacs =
    let emacsWithImageMagick = with pkgs; recurseIntoAttrs (emacsPackagesNgGen (pkgs.emacs.override { imagemagick = pkgs.imagemagick; })); in
    with pkgs.emacsPackages; emacsWithImageMagick.emacsWithPackages
      [ pkgs.haskellPackages.ghc-mod haskellMode magit emacsw3m ];
  myhaskell = pkgs.haskellPackages.ghcWithPackages (p: with p; [
    binary byteorder bytestring containers exceptions free lens mmorph mtl
    PhiledCommon pipes pipes-binary pipes-bytestring pipes-safe QuickCheck sdl2
    sdl2-ttf transformers unix
    cabal-install hlint
  ]);
in pkgs.stdenv.mkDerivation {
  name = "GUIS";
  buildInputs = [ myemacs myhaskell pkgs.dejavu_fonts ];
  fonts = pkgs.dejavu_fonts;
  shellHook = ''
    emacs-tcp guis .emacs
  '';
}
