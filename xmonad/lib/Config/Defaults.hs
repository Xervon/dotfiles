module Config.Defaults
( Workspace (..)
, autostart
, bar
, barAlpha
, barHeight
, barWidthPercent
, terminal
, lock
, editor
, snapshot
, volumeGui
, jackGui
, musicPlayer
, font
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
, modMask
, workspaces
, smallMonWidth
, nav2DConf
) where

import qualified Config.Colors as CC

import Font
import Color

import GHC.Word (Word32)

import XMonad (Dimension, KeyMask, mod4Mask)
import XMonad.Actions.Navigation2D
import XMonad.Layout.Spacing (Border (..))
import XMonad.Prompt hiding (font, promptBorderWidth)

data Workspace
  = ZERO
  | DEV
  | QUTE
  | WEB
  | FOUR
  | FIVE
  | SIX
  | SEVEN
  | EIGHT
  | NINE
  deriving (Enum, Eq, Show, Read)
workspaces                   = [ DEV, QUTE, WEB, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE, ZERO ]

autostart                    =
                             [ "/usr/bin/picom &"
                             , "/usr/bin/unclutter &"
                             , "/usr/bin/xautolock &"
                             , "/usr/bin/trayer-srg --edge top --align right --width " ++ show (100 - barWidthPercent) ++ " --heighttype pixel --height " ++ show barHeight ++ " --transparent true --tint " ++ showCColor CC.base03 ++ " --alpha " ++ show (255 - barAlpha) ++ " &"
                             ]

bar                          = "\"$HOME/.xmonad/xmobar\""
barAlpha                     = 215
barHeight                    = 30
barWidthPercent              = 95

-- terminal                     = "urxvtc -e tmux"
terminal                     = "alacritty"
lock                         = "slock"
editor                       = "emacsclient -c"
snapshot                     = "flameshot gui"

volumeGui                    = ("dex /usr/share/applications/pavucontrol.desktop", "Pavucontrol")
jackGui                      = ("dex /usr/share/applications/qjackctl.desktop"   , "QJackCtl")
musicPlayer                  = ("dex /usr/share/applications/spotify.desktop"    , "Spotify")

font                         = Font "SourceCode Pro" 20 Regular True

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

modMask                      = mod4Mask

smallMonWidth                = 1920

nav2DConf                    = def
  { defaultTiledNavigation = centerNavigation
  , floatNavigation        = centerNavigation
  , screenNavigation       = lineNavigation
  , layoutNavigation       = [("Full", centerNavigation)]
  , unmappedWindowRect     = [("Full", singleWindowRect)]
  }

autostart                    :: [String]

bar                          :: String
barAlpha                     :: Int
barHeight                    :: Int
barWidthPercent              :: Int

terminal                     :: String
lock                         :: String
editor                       :: String
snapshot                     :: String

volumeGui                    :: (String, String)
jackGui                      :: (String, String)
musicPlayer                  :: (String, String)

font                         :: Font

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

modMask                      :: KeyMask
workspaces                   :: [Workspace]

smallMonWidth                :: Word32

nav2DConf                    :: Navigation2DConfig
