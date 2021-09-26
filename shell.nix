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
in pkgs.mkShell {
  buildInputs = with pkgs; [
    bash
    vim

    ghc
    haskellPackages.stack
    haskellPackages.elsa # Bring marked as broken, but usable. Probably elsa's test cases?
   
    nixStable

    niv
  ];

  shellHook = ''
    # Before run command
    echo 'CSE230 - Course Env - Elsa'
    echo 'Use `elsa file` to exec'
    echo 'check makefile for more info'
  '';
}
