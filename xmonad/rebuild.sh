#!/usr/bin/env sh

cd "$(dirname $0)"
ghc -threaded -ilib/ -o xmonad-x86_64-linux xmonad.hs
ghc -threaded -ilib/ xmobar.hs
ghc -threaded -ilib/ termonad.hs
