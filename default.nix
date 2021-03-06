{ mkDerivation, acid-state, attoparsec, base, blaze-html, cereal
, clckwrks, containers, directory, filepath, gd, happstack-server
, hsp, hsx2hs, ixset, magic, mtl, reform, reform-happstack
, reform-hsp, safecopy, stdenv, text, web-plugins, web-routes
, web-routes-th, file
}:
mkDerivation {
  pname = "clckwrks-plugin-media";
  version = "0.6.14";
  src = ./.;
  libraryHaskellDepends = [
    acid-state attoparsec base blaze-html cereal clckwrks containers
    directory filepath gd happstack-server hsp ixset magic mtl reform
    reform-happstack reform-hsp safecopy text web-plugins web-routes
    web-routes-th
  ];
  extraLibraries = [ file ];
  libraryToolDepends = [ hsx2hs ];
  homepage = "http://clckwrks.com/";
  description = "media plugin for clckwrks";
  license = stdenv.lib.licenses.bsd3;
}
