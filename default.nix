{ pkgs ? (import <nixpkgs> {}).pkgs
}:
let
  profiledHaskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      mkDerivation = args: super.mkDerivation (args // {
        enableLibraryProfiling = true;
      });
    };
  };
  philedcommonsha = "8e8af59baf816e80e74eee533900c299b426169bd1787bf4dcc46a9d18a31e1d";
  myemacs =
    let emacsWithImageMagick = with pkgs; recurseIntoAttrs (emacsPackagesNgGen (pkgs.emacs.override { imagemagick = pkgs.imagemagick; })); in
    with pkgs.emacsPackages; emacsWithImageMagick.emacsWithPackages
      [ profiledHaskellPackages.ghc-mod haskellMode magit emacsw3m ];
  PhiledCommon = profiledHaskellPackages.callPackage /home/phil/PhiledCommon.nix {};
  sdlm = profiledHaskellPackages.callPackage /home/phil/SDLM.nix { PhiledCommon = PhiledCommon; };
  myhaskell = profiledHaskellPackages.ghcWithPackages (p: with p; [
    binary byteorder bytestring containers exceptions free lens mmorph mtl
    PhiledCommon sdlm pipes pipes-binary pipes-bytestring pipes-safe QuickCheck sdl2
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
