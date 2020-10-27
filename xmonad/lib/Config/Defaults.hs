module Config.Defaults
( Workspace (..)
, ScratchpadConfig (..)
, autostart
, bar
, barAlpha
, barHeight
, barWidthPercent
, shell
, terminal
, lock
, autolockToggle
, editor
, webBrowser
, snapshot
, fileBrowser
, mail
, namedScratchpads
, font
, emojiFont
, smartGaps
, outerGap
, isOuterGap
, innerGap
, isInnerGap
, windowBorder
, normalWindowBorderColor
, focusedWindowBorderColor
, focusFollowsMouse
, clickJustFocuses
, topBarHeight
, promptPosition
, promptBorderWidth
, promptAutoCompleteTimer
, unicodeDataPath
, modMask
, workspaces
, smallMonWidth
, nav2DConf
) where

import qualified Config.Colors as CC

import Font
import Color

import GHC.Word (Word32)

import XMonad (Dimension, KeyMask, mod4Mask, X)
import XMonad.Actions.Navigation2D
import XMonad.Config.Prime (ManageHook, Query, (=?))
import XMonad.Layout.Spacing (Border (..))
import XMonad.ManageHook (className, appName)
import XMonad.Prompt hiding (font, promptBorderWidth)
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce

data Workspace
  = ZERO
  | DEV
  | TWO
  | WEB
  | FOUR
  | FIVE
  | SIX
  | SEVEN
  | EIGHT
  | NINE
  deriving (Enum, Eq, Show, Read)
workspaces                   = [ DEV, TWO, WEB, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE, ZERO ]

data ScratchpadConfig = ScratchpadConfig
  { name    :: String
  , command :: String
  , idPred  :: Query Bool
  , manage  :: ManageHook
  , keybind :: String
  , help    :: String
  }

autostart                    =
  [ (spawnOnce,              "/usr/bin/picom &")
  , (spawnOnce,              "/usr/bin/unclutter &")
  , (spawnOnce,              "/usr/bin/trayer-srg --edge top --align right --width " ++ show (100 - barWidthPercent) ++ " --heighttype pixel --height " ++ show barHeight ++ " --transparent true --tint " ++ showCColor CC.base03 ++ " --alpha " ++ show (255 - barAlpha) ++ " &")
  , (spawnOnce,              "/usr/bin/dex /usr/share/applications/qjackctl.desktop")

  , (spawnOnce,              "/usr/bin/dex /usr/share/applications/steam.desktop")
  ]

-- bar                          = "/usr/bin/taffybar"
bar                          = "$HOME/.xmonad/xmobar"
barAlpha                     = 215
barHeight                    = 30
barWidthPercent              = 95

shell                        = [ "/usr/bin/tmux" ]
terminal                     = "$HOME/.xmonad/termonad"
lock                         = "/usr/bin/env dm-tool lock"
autolockToggle               = "$HOME/.cache/dotfiles/bin/autolock toggle"
editor                       = "/usr/bin/env emacsclient -s default -c"
webBrowser                   = "/usr/bin/env qutebrowser -r default"
snapshot                     = "/usr/bin/env flameshot gui"
fileBrowser                  = "/usr/bin/env xterm -e '/usr/bin/env ranger'"
mail                         = "/usr/bin/env emacsclient -s default -cue '(notmuch-search-unread)'"

namedScratchpads             =
  [ ScratchpadConfig "volumegui"  "/usr/bin/env dex /usr/share/applications/pavucontrol.desktop"                                                                                             (className =? "Pavucontrol"  ) defaultFloating "M-v"   "Pulse Config"
  , ScratchpadConfig "spotify"    "/usr/bin/env dex /usr/share/applications/spotify.desktop"                                                                                                 (className =? "Spotify"      ) nonFloating     "M-s"   "Spotify"
  , ScratchpadConfig "steam"      "/usr/bin/env dex /usr/share/applications/steam.desktop"                                                                                                   (className =? "Steam"        ) nonFloating     "M-S-s" "Steam"
  , ScratchpadConfig "calculator" "/usr/bin/env dex /usr/share/applications/qalculate-gtk.desktop"                                                                                           (className =? "Qalculate-gtk") defaultFloating "M-c"   "Calculator"
  , ScratchpadConfig "whatsapp"   "/usr/bin/env qutebrowser --qt-arg name whatsapp --target window -B \"$HOME/.local/share/qutebrowser-whatsapp\" -R \"https://web.whatsapp.com\"" (appName =? "whatsapp")        nonFloating     "M-w"   "WhatsApp"
  ]

font                         = Font "SourceCode Pro" 13 Regular True
emojiFont                    = Font "Noto Emoji" 13 Regular True

smartGaps                    = True
outerGap                     = Border 5 5 5 5
isOuterGap                   = True
innerGap                     = Border 5 5 5 5
isInnerGap                   = True

windowBorder                 = 0
normalWindowBorderColor      = CC.focusColor
focusedWindowBorderColor     = CC.unfocusColor

focusFollowsMouse            = False
clickJustFocuses             = True

topBarHeight                 = 10

promptPosition               = Top
promptBorderWidth            = 0
promptAutoCompleteTimer      = Just 500

unicodeDataPath              = "/usr/share/unicode-data/UnicodeData.txt"

modMask                      = mod4Mask

smallMonWidth                = 1920

nav2DConf                    = def
  { defaultTiledNavigation = centerNavigation
  , floatNavigation        = centerNavigation
  , screenNavigation       = lineNavigation
  , layoutNavigation       = [("Full", centerNavigation)]
  , unmappedWindowRect     = [("Full", singleWindowRect)]
  }

autostart                    :: [(String -> X (), String)]

bar                          :: String
barAlpha                     :: Int
barHeight                    :: Int
barWidthPercent              :: Int

shell                        :: [String]
terminal                     :: String
lock                         :: String
autolockToggle               :: String
editor                       :: String
webBrowser                   :: String
snapshot                     :: String
fileBrowser                  :: String
mail                         :: String

namedScratchpads             :: [ScratchpadConfig]

font                         :: Font
emojiFont                    :: Font

smartGaps                    :: Bool
outerGap                     :: Border
isOuterGap                   :: Bool
innerGap                     :: Border
isInnerGap                   :: Bool

windowBorder                 :: Dimension
normalWindowBorderColor      :: Color
focusedWindowBorderColor     :: Color

focusFollowsMouse            :: Bool
clickJustFocuses             :: Bool

topBarHeight                 :: Word32

promptPosition               :: XPPosition
promptBorderWidth            :: Dimension
promptAutoCompleteTimer      :: Maybe Int

unicodeDataPath              :: String

modMask                      :: KeyMask
workspaces                   :: [Workspace]

smallMonWidth                :: Word32

nav2DConf                    :: Navigation2DConfig
