{ mkDerivation, aeson, base, bytestring, containers, dataenc
, download-curl, stdenv, unordered-containers
}:
mkDerivation {
  pname = "GoogleDirections";
  version = "0.3.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers dataenc download-curl
    unordered-containers
  ];
  description = "Haskell Interface to Google Directions API";
  license = stdenv.lib.licenses.bsd3;
}
