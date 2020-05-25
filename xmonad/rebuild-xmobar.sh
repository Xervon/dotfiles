#!/usr/bin/env sh

cd "$(dirname $0)"
ghc -threaded -ilib/ xmobar.hs
