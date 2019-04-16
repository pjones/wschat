{ mkDerivation, aeson, base, concurrent-supply, directory, filepath
, http-types, stdenv, stm, text, unordered-containers, wai
, wai-websockets, warp, websockets
}:
mkDerivation {
  pname = "wschat";
  version = "0.1.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base concurrent-supply directory filepath http-types stm text
    unordered-containers wai wai-websockets warp websockets
  ];
  description = "Simple WebSocket chat server";
  license = stdenv.lib.licenses.bsd2;
}
