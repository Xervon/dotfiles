module Config.Hooks
( manageHook
, eventHook
, logHook
, startupHook
, LibNotifyUrgencyHook (..)
) where

import qualified Config.Colors      as CC
import qualified Config.Defaults    as C
import qualified Config.ScratchPads as SP

import Color

import Data.Monoid
import GHC.IO.Handle

import XMonad (gets, Event, handleEventHook, ManageHook, spawn, windowset, X, XConfig)
import XMonad.Actions.CopyWindow
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Layout.Fullscreen
import XMonad.ManageHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad hiding (name)
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

forceCenterFloat :: ManageHook
forceCenterFloat = doFloatDep move
  where
    move :: W.RationalRect -> W.RationalRect
    move _ = W.RationalRect x y w h

    w, h, x, y :: Rational
    w = 1/3
    h = 1/2
    x = (1-w)/2
    y = (1-h)/2

manageHook :: ManageHook
manageHook =
      manageSpecific
  <+> manageDocks
  <+> namedScratchpadManageHook SP.scratchPads
  <+> fullscreenManageHook
  <+> manageSpawn
  where
    manageSpecific = composeAll
      [ isDialog                   --> forceCenterFloat
      -- TODO: not working yet
      , className =? "QJackCtl"    --> forceCenterFloat
      , className =? "Pavucontrol" --> forceCenterFloat
      ]

eventHook :: XConfig l -> Event -> X All
eventHook conf = docksEventHook
          <+> fadeWindowsEventHook
          <+> dynamicTitle myDynHook
          <+> handleEventHook conf
          <+> XMonad.Layout.Fullscreen.fullscreenEventHook
  where
    myDynHook = composeAll []

logHook :: Handle -> X ()
logHook h = do
  copies <- wsContainingCopies
  let check ws | ws `elem` copies = pad . xmobarColor (showWebColor CC.yellow) (showWebColor CC.red) . wrap "*" " "  $ ws
               | otherwise = pad ws

  fadeWindowsLogHook fadeHook
  ewmhDesktopsLogHook
  dynamicLogWithPP $ def
    { ppCurrent             = xmobarColor (showWebColor CC.active) "" . wrap "[" "]"
    , ppTitle               = xmobarColor (showWebColor CC.active) "" . shorten 50
    , ppVisible             = xmobarColor (showWebColor CC.base0)  "" . wrap "(" ")"
    , ppUrgent              = xmobarColor (showWebColor CC.red)    "" . wrap " " " "
    , ppHidden              = check
    , ppHiddenNoWindows     = const ""
    , ppSep                 = xmobarColor (showWebColor CC.red) (showWebColor CC.blue) "  :  "
    , ppWsSep               = " "
    , ppLayout              = xmobarColor (showWebColor CC.yellow) ""
    , ppOrder               = id
    , ppOutput              = hPutStrLn h
    , ppSort                = fmap
                              (namedScratchpadFilterOutWorkspace.)
                              (ppSort def)
    , ppExtras              = []
    }

fadeHook :: FadeHook
fadeHook = composeAll
 [ opaque
  , isUnfocused --> opacity 0.85
  , (className =? "URxvt") <&&> isUnfocused --> opacity 0.9
  , (className =? "trayer") --> opaque
  , (className =? "xmobar") --> opaque
  , isDialog --> opaque
  ]

startupHook :: X ()
startupHook = do
  mapM_ spawnOnce C.autostart

-- from https://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)
instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name     <- getName w
    Just idx <- fmap (W.findTag w) $ gets windowset
    safeSpawn "notify-send" [show name, "workspace " ++ idx]
