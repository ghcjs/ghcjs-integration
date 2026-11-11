-- GHC JavaScript backend build (ghc914-sh cross), kept apart from the native
-- cabal.project.local the default dev shell manages:
--   nix develop '.?submodules=1#javascript-unknown-ghcjs'
--   cabal build --project-file=cabal.project.js ghcjs-dom
-- cabal.project.js.local is that shell's haskell-nix-cabal-project-local-sync source.
import: cabal.project
