module Config.ScratchPads
( scratchPads
) where

import qualified Config.Defaults as C

import XMonad.ManageHook
import XMonad.Util.NamedScratchpad

scratchPads :: [NamedScratchpad]
scratchPads =
  [ (NS "audioControl" (fst C.nsVolumeGui)   (className =? (snd C.nsVolumeGui)))   defaultFloating
  , (NS "musicPlayer"  (fst C.nsMusicPlayer) (className =? (snd C.nsMusicPlayer))) nonFloating
  , (NS "steam"        (fst C.nsSteam)       (className =? (snd C.nsSteam)))       nonFloating
  , (NS "calculator"   (fst C.nsCalculator)  (className =? (snd C.nsCalculator)))  defaultFloating
  , (NS "whatsapp"     (fst C.nsWhatsApp)    (className =? (snd C.nsWhatsApp)))    nonFloating
  ]
