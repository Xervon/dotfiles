module Config.Prompts
( defaultPrompt
, warnPrompt
, dangerPrompt
, fuzzy
) where

import qualified Config.Colors   as CC
import qualified Config.Defaults as C

import Color

import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch

defaultPrompt :: XPConfig
defaultPrompt = def
  { font                  = show C.font
  , bgColor               = showWebColor CC.base03
  , fgColor               = showWebColor CC.active
  , fgHLight              = showWebColor CC.base03
  , bgHLight              = showWebColor CC.active
  , borderColor           = showWebColor CC.base03
  , promptBorderWidth     = C.promptBorderWidth
  , height                = fromIntegral C.barHeight
  , position              = C.promptPosition
  }

warnPrompt :: XPConfig
warnPrompt    = defaultPrompt
  { bgColor = showWebColor CC.yellow
  , fgColor = showWebColor CC.base03
  }

dangerPrompt :: XPConfig
dangerPrompt  = defaultPrompt
  { bgColor = showWebColor CC.red
  , fgColor = showWebColor CC.base3
  }

fuzzy :: XPConfig -> XPConfig
fuzzy c = c { searchPredicate = fuzzyMatch
            , sorter          = fuzzySort
            }
