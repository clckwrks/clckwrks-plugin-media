{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, acid-state, attoparsec, base, blaze-html
      , cereal, clckwrks, containers, directory, filepath, gd
      , happstack-server, hsp, hsx2hs, ixset, magic, mtl, reform
      , reform-happstack, reform-hsp, safecopy, stdenv, text, web-plugins
      , web-routes, web-routes-th, cabal-install
      }:
      mkDerivation {
        pname = "clckwrks-plugin-media";
        version = "0.6.15";
        src = ./.;
        libraryHaskellDepends = [
          acid-state attoparsec base blaze-html cereal clckwrks containers
          directory filepath gd happstack-server hsp ixset magic mtl reform
          reform-happstack reform-hsp safecopy text web-plugins web-routes
          web-routes-th cabal-install
        ];
        libraryToolDepends = [ hsx2hs ];
        homepage = "http://clckwrks.com/";
        description = "media plugin for clckwrks";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
