{config, pkgs, ...}: {
  compiler-nix-name = "ghc982";
  modules = [({pkgs, ...}: {
    packages.webkit2gtk3-javascriptcore.components.library.doHaddock = false;
  })];
  flake.variants.ghc8107.compiler-nix-name = pkgs.lib.mkForce "ghc8107";
  shell.buildInputs = [ pkgs.nodejs ];
  shell.tools.cabal = {};
  crossPlatforms = p: pkgs.lib.optional (config.compiler-nix-name == "ghc8107" || __compareVersions pkgs.haskell-nix.compiler.${config.compiler-nix-name}.version "9.6.4" >=0) p.ghcjs;

  # Use this for checking if `aeson` 2 works (tests will not build because `webdriver` still needs aeson <2)
  cabalProjectLocal = ''
    constraints: aeson >=2
  '';
}
