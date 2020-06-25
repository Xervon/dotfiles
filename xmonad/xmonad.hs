import qualified Config.Colors   as CC
import qualified Config.Defaults as C
import qualified Config.Hooks              as H
import qualified Config.Layouts            as L
import qualified Config.Keybinds           as K

import Color

import XMonad
import XMonad.Actions.Navigation2D
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedActions
import XMonad.Util.Run
import XMonad.Wallpaper

import System.Taffybar.Support.PagerHints (pagerHints)

main = do
  xmproc <- spawnPipe C.bar
  setRandomWallpaper [ "$HOME/.dotfiles/bg" ]
  xmonad
    $ withNavigation2DConfig C.nav2DConf
    $ withUrgencyHook H.LibNotifyUrgencyHook
    $ ewmh
    $ pagerHints
    $ addDescrKeys' ((C.modMask, xK_F1), K.showKeybindings) K.keybinds
    $ (myConfig xmproc)
        { handleEventHook    = H.eventHook $ myConfig xmproc
        }

myConfig p = def
  { terminal           = C.terminal
  , focusFollowsMouse  = C.focusFollowsMouse
  , clickJustFocuses   = C.clickJustFocuses
  , borderWidth        = C.windowBorder
  , normalBorderColor  = showWebColor C.normalWindowBorderColor
  , focusedBorderColor = showWebColor C.focusedWindowBorderColor

  , modMask            = C.modMask
  , workspaces         = map show C.workspaces

  , mouseBindings      = K.mousebinds

  , layoutHook         = L.layoutHook
  , manageHook         = H.manageHook
  , logHook            = H.logHook p
  , startupHook        = H.startupHook
  }
