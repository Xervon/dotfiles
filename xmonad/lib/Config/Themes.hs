module Config.Themes
( topBarTheme
, tabTheme
) where

import Config.Colors   as CC
import Config.Defaults as C

import Color

import XMonad.Layout.Decoration
import XMonad.Layout.ShowWName

topBarTheme :: Theme
topBarTheme = def
  { fontName              = "-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso8859-1" -- show C.font
  , inactiveBorderColor   = showWebColor CC.base03
  , inactiveColor         = showWebColor CC.base03
  , inactiveTextColor     = showWebColor CC.base03
  , activeBorderColor     = showWebColor CC.active
  , activeColor           = showWebColor CC.active
  , activeTextColor       = showWebColor CC.active
  , urgentBorderColor     = showWebColor CC.red
  , urgentTextColor       = showWebColor CC.yellow
  , decoHeight            = C.topBarHeight
  }

tabTheme :: Theme
tabTheme = def
  { fontName              = "-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso8859-1" -- show C.font
  , activeColor           = showWebColor CC.active
  , inactiveColor         = showWebColor CC.base02
  , activeBorderColor     = showWebColor CC.active
  , inactiveBorderColor   = showWebColor CC.base02
  , activeTextColor       = showWebColor CC.base03
  , inactiveTextColor     = showWebColor CC.base00
  }
