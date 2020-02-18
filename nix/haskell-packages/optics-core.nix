{ mkDerivation, array, base, containers, indexed-profunctors
, stdenv, transformers
}:
mkDerivation {
  pname = "optics-core";
  version = "0.2";
  sha256 = "287ff8c28cba855aa9d247f3ed6d476338039653e99a2f3312f9f5ce51571aa6";
  libraryHaskellDepends = [
    array base containers indexed-profunctors transformers
  ];
  description = "Optics as an abstract interface: core definitions";
  license = stdenv.lib.licenses.bsd3;
}
