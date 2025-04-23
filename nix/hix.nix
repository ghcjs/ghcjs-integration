{config, pkgs, ...}: {
  compiler-nix-name = "ghc912";
  modules = [({pkgs, ...}: {
    package-keys = ["webkit2gtk3-javascriptcore"];
    packages.webkit2gtk3-javascriptcore.components.library.doHaddock = false;
  })];
  flake.variants.ghc967.compiler-nix-name = pkgs.lib.mkForce "ghc967";
  shell.buildInputs = [ pkgs.pkgsBuildBuild.nodejs ];
  shell.tools.cabal = {};
  crossPlatforms = p: [ p.ghcjs ];

  # Use this for checking if `aeson` 2 works (tests will not build because `webdriver` still needs aeson <2)
  cabalProjectLocal = ''
    constraints: aeson >=2
  '';
}
