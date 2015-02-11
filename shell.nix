let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages_ghc784.override {
    extension = self: super: {
      thispackage = self.callPackage ./default.nix {};
    };
  };
in pkgs.myEnvFun {
     name = haskellPackages.thispackage.name;
     buildInputs = 
     [
       (haskellPackages.ghcWithPackages (hs: ([
         hs.cabalInstall
         hs.ghcMod
         hs.hasktags
         hs.hlint
         hs.present
         hs.stylishHaskell
         hs.hoogleLocal
         hs.happy
       ] ++ hs.thispackage.propagatedNativeBuildInputs)))
     ];
   } 

