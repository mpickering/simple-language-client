with { pkgs = import <nixpkgs> {}; };
pkgs.mkShell
  { buildInputs = [ pkgs.haskell.compiler.ghc865 pkgs.zlib pkgs.haskellPackages.cabal-install pkgs.icu ];
    shellHook=''export LD_LIBRARY_PATH=${pkgs.gmp}/lib:${pkgs.zlib}/lib:${pkgs.ncurses}/lib:${pkgs.icu}/lib'';
  }
