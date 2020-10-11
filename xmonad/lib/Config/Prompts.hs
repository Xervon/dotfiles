module Config.Prompts
( defaultPrompt
, warnPrompt
, dangerPrompt
, fuzzy
, autocomplete
) where

import qualified Config.Colors   as CC
import qualified Config.Defaults as C

import Color

import XMonad
import XMonad.Prompt
import XMonad.Prompt.CustomFuzzyMatch

import Control.Arrow (first)
import qualified Data.Map as M
import Data.Char (isSpace)

myPromptKeymap :: M.Map (KeyMask, KeySym) (XP ())
myPromptKeymap = M.fromList $
  map (first $ (,) controlMask)
  [ (xK_l,         setSuccess True >> setDone True)
  , (xK_w,         killWord Prev)
  , (xK_g,         quit)
  ] ++
  map (first $ (,) mod1Mask)
  [ (xK_f,         moveWord' wordPred Next)
  , (xK_b,         moveWord' wordPred Prev)
  , (xK_d,         killWord Next)
  ] ++
  map (first $ (,) 0)
  [ (xK_Return,    setSuccess True >> setDone True)
  , (xK_KP_Enter,  setSuccess True >> setDone True)
  , (xK_BackSpace, deleteString Prev)
  , (xK_Delete,    deleteString Next)
  , (xK_Home,      startOfLine)
  , (xK_End,       endOfLine)
  , (xK_Escape,    quit)
  ]
  where wordPred c = or . (map $ \p -> p c) $ [isSpace, (== '/')]

defaultPrompt :: XPConfig
defaultPrompt = def
  { font                  = "xft:" ++ show C.font ++ "," ++ show C.emojiFont
  , bgColor               = showWebColor CC.base03
  , fgColor               = showWebColor CC.active
  , fgHLight              = showWebColor CC.base03
  , bgHLight              = showWebColor CC.active
  , borderColor           = showWebColor CC.base03
  , promptBorderWidth     = C.promptBorderWidth
  , height                = fromIntegral C.barHeight
  , position              = C.promptPosition
  , promptKeymap          = myPromptKeymap
  , completionKey         = (controlMask, xK_j)
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
fuzzy c = c { searchPredicate = customFuzzyMatch
            , sorter          = customFuzzySort
            }

autocomplete :: XPConfig -> XPConfig
autocomplete c = c { autoComplete = C.promptAutoCompleteTimer
                   }
