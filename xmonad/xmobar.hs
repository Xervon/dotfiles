import qualified Config.Colors   as CC
import qualified Config.Defaults as C

import Color

import Xmobar

config = defaultConfig
  { font              = "xft:" ++ show C.font
  , bgColor           = showWebColor $ CC.base03 { g = b CC.base03, b = g CC.base03 }
  , fgColor           = showWebColor CC.base0
  , alpha             = C.barAlpha
  , position          = OnScreen 0 $ TopSize L C.barWidthPercent C.barHeight
  , overrideRedirect  = True
  , allDesktops       = True
  , commands          = [ Run $ Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10
                        , Run $ Network "enp6s0" ["-L","0","-H","32","--normal","green","--high","red"] 10
                        , Run $ Memory ["-t","Mem: <usedratio>%"] 10
                        , Run $ Date "%a %b %_d %Y %H:%M:%S" "date" 10
                        , Run $ Com "uname" ["-r"] "" 0
                        , Run $ StdinReader
                        ]
  , sepChar           = "%"
  , alignSep          = "}{"
  , template          = "%StdinReader% }{ %cpu% | %memory% | %enp6s0% * <fc=#ee9a00>%uname%</fc> * <fc=#ee9a00>%date%</fc> "
  }

main :: IO ()
main = xmobar config
