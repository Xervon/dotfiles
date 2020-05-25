module Config.Colors
( base00
, base01
, base02
, base03
, base0
, base1
, base2
, base3
, yellow
, orange
, red
, magenta
, violet
, blue
, cyan
, green
, active
, activeWarn
, inactive
, focusColor
, unfocusColor
) where

import Color

base03       = Color 0x00 0x2b 0x36
base02       = Color 0x07 0x36 0x42
base01       = Color 0x58 0x6e 0x75
base00       = Color 0x65 0x7b 0x83
base0        = Color 0x83 0x94 0x96
base1        = Color 0x93 0xa1 0xa1
base2        = Color 0xee 0xe8 0xd5
base3        = Color 0xfd 0xf6 0xe3
yellow       = Color 0xb5 0x89 0x00
orange       = Color 0xcb 0x4b 0x16
red          = Color 0xdc 0x32 0x2f
magenta      = Color 0xd3 0x36 0x82
violet       = Color 0x6c 0x71 0xc4
blue         = Color 0x26 0x8b 0xd2
cyan         = Color 0x2a 0xa1 0x98
green        = Color 0x85 0x99 0x00

active       = blue
activeWarn   = red
inactive     = base02
focusColor   = blue
unfocusColor = base02

base03       :: Color
base02       :: Color
base01       :: Color
base00       :: Color
base0        :: Color
base1        :: Color
base2        :: Color
base3        :: Color
yellow       :: Color
orange       :: Color
red          :: Color
magenta      :: Color
violet       :: Color
blue         :: Color
cyan         :: Color
green        :: Color

active       :: Color
activeWarn   :: Color
inactive     :: Color
focusColor   :: Color
unfocusColor :: Color
