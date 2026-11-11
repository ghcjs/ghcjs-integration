{config, pkgs, ...}: {
#  builderVersion = 2;
  # ghc914-sh: the stable-haskell GHC 9.14 (haskell.nix master), as in leksah.
  compiler-nix-name = "ghc914-sh";
  modules = [({pkgs, ...}: {
    package-keys = ["webkit2gtk3-javascriptcore"];
    packages.webkit2gtk3-javascriptcore.components.library.doHaddock = false;
    enableStatic = !pkgs.stdenv.hostPlatform.isGhcjs;
  })];
  flake.variants.v2.builderVersion = 2;
  flake.variants.ghc914.compiler-nix-name = pkgs.lib.mkForce "ghc914";
  # Upstream GHC 9.14 needs these built from source (they conflict with the
  # boot package pins ghc914-sh uses, so they are only set for this variant).
  flake.variants.ghc914.cabalProjectLocal = pkgs.lib.mkForce ''
    constraints: aeson >=2,
                 exceptions source,
                 filepath source,
                 os-string source,
                 file-io source,
                 time source,
                 hashable source,
                 unordered-containers source,
                 directory source
  '';
  flake.variants.ghc912.compiler-nix-name = pkgs.lib.mkForce "ghc912";
  flake.variants.ghc967.compiler-nix-name = pkgs.lib.mkForce "ghc967";
  shell.buildInputs = [ pkgs.pkgsBuildBuild.nodejs ];
  shell.tools.cabal = {};
  shell.tools.hoogle.cabalProjectLocal = ''
    constraints: hoogle >= 5
    allow-newer: *:base, *:containers, *:template-haskell
  '';
  crossPlatforms = p: [ p.ghcjs ];

  # Use this for checking if `aeson` 2 works (tests will not build because `webdriver` still needs aeson <2)
  cabalProjectLocal = ''
    constraints: aeson >=2
  '';
}
