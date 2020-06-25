module Config.Defaults
( Workspace (..)
, autostart
, bar
, barAlpha
, barHeight
, barWidthPercent
, shell
, terminal
, lock
, editor
, webBrowser
, snapshot
, nsVolumeGui
, nsMusicPlayer
, nsSteam
, nsCalculator
, nsWhatsApp
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

import XMonad (Dimension, KeyMask, mod4Mask, X)
import XMonad.Actions.Navigation2D
import XMonad.Layout.Spacing (Border (..))
import XMonad.Prompt hiding (font, promptBorderWidth)
import XMonad.Util.SpawnOnce

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
                             [ (spawnOnce,              "/usr/bin/picom &")
                             , (spawnOnce,              "/usr/bin/unclutter &")
                             , (spawnOnce,              "/usr/bin/xautolock &")
                             , (spawnOnce,              "/usr/bin/flameshot &")
                             , (spawnOnce,              "/usr/bin/trayer-srg --edge top --align right --width " ++ show (100 - barWidthPercent) ++ " --heighttype pixel --height " ++ show barHeight ++ " --transparent true --tint " ++ showCColor CC.base03 ++ " --alpha " ++ show (255 - barAlpha) ++ " &")
                             , (spawnOnce,              "/usr/bin/dex /usr/share/applications/qjackctl.desktop")

                             , (spawnOnce,              "/usr/bin/dex /usr/share/applications/steam.desktop")
                             , (spawnOnOnce $ show WEB, "/usr/bin/dex /usr/share/applications/firefox.desktop")
                             ]

-- bar                          = "/usr/bin/taffybar"
bar                          = "\"$HOME/.xmonad/xmobar\""
barAlpha                     = 215
barHeight                    = 30
barWidthPercent              = 95

shell                        = [ "/usr/bin/tmux" ]
-- terminal                     = "urxvtc -e " ++ (unwords shell)
-- terminal                     = "alacritty"
terminal                     = "$HOME/.xmonad/termonad"
lock                         = "/usr/bin/slock"
editor                       = "/usr/bin/emacsclient -c"
webBrowser                   = "dex /usr/share/applications/firefox.desktop"
snapshot                     = "/usr/bin/flameshot gui"

nsVolumeGui                  = ("dex /usr/share/applications/pavucontrol.desktop"  , "Pavucontrol")
nsMusicPlayer                = ("dex /usr/share/applications/spotify.desktop"      , "Spotify")
nsSteam                      = ("dex /usr/share/applications/steam.desktop"        , "Steam")
nsCalculator                 = ("dex /usr/share/applications/qalculate-gtk.desktop", "Qalculate-gtk")
nsWhatsApp                   = ("/usr/bin/env surf -LW 'whatsapp' 'https://web.whatsapp.com'", "whatsapp")

font                         = Font "SourceCode Pro" 13 Regular True

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

autostart                    :: [(String -> X (), String)]

bar                          :: String
barAlpha                     :: Int
barHeight                    :: Int
barWidthPercent              :: Int

shell                        :: [String]
terminal                     :: String
lock                         :: String
editor                       :: String
webBrowser                   :: String
snapshot                     :: String

nsVolumeGui                  :: (String, String)
nsMusicPlayer                :: (String, String)
nsSteam                      :: (String, String)
nsCalculator                 :: (String, String)

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
