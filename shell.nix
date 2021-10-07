{ sources ? import ./nix/sources.nix }:     # import the sources
with
{ overlay = _: pkgs:
    {
      niv = (import sources.niv {}).niv;
    };
};
let

  pkgs = (import sources.nixpkgs) {
    overlays = [ overlay ];
    config.allowUnfree = true;
    config.allowBroken = true;
  };

  # f = pkgs.lib.id (pkgs.haskell.packages.ghc884.callPackage ./cabal-pkg.nix {});
in pkgs.mkShell {

  # inputsFrom = [ f ];
  buildInputs = with pkgs; [
    bash
    vim

    ghc
    # vscodium
    vscode
    nixStable

    cabal2nix

    niv
  ];

  shellHook = ''
    # Before run command
    echo 'CSE230 - Course Env - GHC'
    echo 'check makefile for more info'
  '';
}
