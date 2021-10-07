{ mkDerivation, base, bmp, directory, filepath, gloss, gloss-export
, htdp-image, JuicyPixels, process, QuickCheck, stdenv, tasty
, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "cse230-trees";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base directory filepath gloss gloss-export htdp-image JuicyPixels
    QuickCheck
  ];
  executableHaskellDepends = [
    base bmp directory filepath gloss gloss-export htdp-image
    JuicyPixels QuickCheck
  ];
  testHaskellDepends = [
    base directory filepath process QuickCheck tasty tasty-hunit
    tasty-quickcheck
  ];
  description = "Starter code";
  license = stdenv.lib.licenses.mit;
}
