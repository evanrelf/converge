{ mkDerivation, aeson, base, base16-bytestring, bytestring
, cryptonite, github, github-webhooks, http-types, memory
, servant, servant-server, stdenv, string-conversions, text
, transformers, unordered-containers, wai, warp
}:
mkDerivation {
  pname = "servant-github-webhook";
  version = "0.4.2.0";
  src = builtins.fetchTarball {
    url = "https://github.com/tsani/servant-github-webhook/archive/89574d30a3bce1411a03d7f8e21606125f4859cb.tar.gz";
    sha256 = "1bqj3p5wrzb5lmcsbyy6qpb2m6yw7jd4zd7ndgb5h0madl0j2l81";
  };
  libraryHaskellDepends = [
    aeson base base16-bytestring bytestring cryptonite github
    github-webhooks http-types memory servant servant-server
    string-conversions text transformers unordered-containers wai
  ];
  testHaskellDepends = [
    aeson base bytestring servant-server text transformers wai warp
  ];
  homepage = "https://github.com/tsani/servant-github-webhook";
  description = "Servant combinators to facilitate writing GitHub webhooks";
  license = stdenv.lib.licenses.mit;
}
