module Config.ScratchPads
( scratchPads
) where

import qualified Config.Defaults as C

import XMonad.ManageHook
import XMonad.Util.NamedScratchpad

scratchPads :: [NamedScratchpad]
scratchPads = map (\c -> NS (C.name c) (C.command c) (C.idPred c) (C.manage c)) C.namedScratchpads
