module Config.ScratchPads
( scratchPads
) where

import qualified Config.Defaults as C

import XMonad.ManageHook
import XMonad.Util.NamedScratchpad

scratchPads :: [NamedScratchpad]
scratchPads =
  [ (NS "audioControl" (fst C.volumeGui)   (className =? (snd C.volumeGui)))   defaultFloating
  , (NS "jackControl"  (fst C.jackGui  )   (className =? (snd C.jackGui)))     defaultFloating
  , (NS "musicPlayer"  (fst C.musicPlayer) (className =? (snd C.musicPlayer))) nonFloating
  ]
