{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bmp, directory, filepath, gloss
      , gloss-export, htdp-image, JuicyPixels, process, QuickCheck
      , stdenv, tasty, tasty-hunit, tasty-quickcheck
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
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
