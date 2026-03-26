{config, pkgs, ...}: {
  compiler-nix-name = "ghc914";
  modules = [({pkgs, ...}: {
    package-keys = ["webkit2gtk3-javascriptcore"];
    packages.webkit2gtk3-javascriptcore.components.library.doHaddock = false;
    enableStatic = !pkgs.stdenv.hostPlatform.isGhcjs;
  })];
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
