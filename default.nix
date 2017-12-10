with import <nixpkgs> {};

let
  myParsec = pkgs.haskell.lib.overrideCabal pkgs.haskellPackages.parsec (drv: {
      version = "3.1.11";
      sha256 = "0vk7q9j2128q191zf1sg0ylj9s9djwayqk9747k0a5fin4f2b1vg";
  });

in
pkgs.haskellPackages.mkDerivation rec {
    pname = "kj";
    version = "0.2.0.2";
    src = ./.;
    license = null;
    buildTools = [ pkgs.coreutils pkgs.cabal-install pkgs.ghc];
    isExecutable = true;

    enableSharedExecutables = false;

    buildDepends = with pkgs.haskellPackages; [
        myParsec

        aeson base bytestring directory filepath process options mtl
    ];
}
