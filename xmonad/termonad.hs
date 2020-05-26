{-# LANGUAGE OverloadedStrings #-}
-- | This is an example Termonad configuration that shows how to use the
-- Solarized colour scheme https://ethanschoonover.com/solarized/

module Main where

import qualified Config.Colors as CC
import qualified Config.Defaults as C
import Color
import Font

import Termonad.Term
import Termonad.Types
import Termonad
  ( CursorBlinkMode(CursorBlinkModeOn)
  , Option(Set)
  , ShowScrollbar(ShowScrollbarNever)
  , TMConfig
  , confirmExit
  , cursorBlinkMode
  , defaultConfigOptions
  , defaultConfigHooks
  , defaultTMConfig
  , options
  , showMenu
  , showScrollbar
  , start
  , FontConfig
  , FontSize(FontSizePoints)
  , defaultFontConfig
  , fontConfig
  , fontFamily
  , fontSize
  )
import Termonad.Config.Colour
  ( AlphaColour
  , ColourConfig
  , Palette(ExtendedPalette)
  , addColourExtension
  , createColourExtension
  , defaultColourConfig
  , backgroundColour
  , foregroundColour
  , palette
  )
import Termonad.Config.Vec (Vec((:*), EmptyVec), N8)
import GI.Gio (noCancellable)
import GI.GLib (SpawnFlags(SpawnFlagsDefault))
import GI.Vte

import Data.Maybe
import Data.Text

myCreateTermHook :: TMState -> Terminal -> IO ()
myCreateTermHook s t = do
  shellPid <-
    terminalSpawnSync
      t
      [PtyFlagsDefault]
      Nothing -- base directory
      C.shell
      Nothing
      ([SpawnFlagsDefault] :: [SpawnFlags])
      Nothing
      noCancellable
  pure ()

myTMConfig :: TMConfig
myTMConfig =
  defaultTMConfig
    { options =
        defaultConfigOptions
          { showScrollbar   = ShowScrollbarNever
          , confirmExit     = False
          , showMenu        = False
          , cursorBlinkMode = CursorBlinkModeOn
          , fontConfig      = fontConf
          }
    , hooks =
        defaultConfigHooks
          { createTermHook  = myCreateTermHook
          }
    }

solarizedDark :: ColourConfig (AlphaColour Double)
solarizedDark =
  defaultColourConfig
    { foregroundColour = Set (getAlphaColour CC.base0)
    , backgroundColour = Set (getAlphaColour CC.base03)
    , palette = ExtendedPalette solarizedDark1 solarizedDark2
    }
  where
    solarizedDark1 :: Vec N8 (AlphaColour Double)
    solarizedDark1 =
         getAlphaColour CC.base02
      :* getAlphaColour CC.red
      :* getAlphaColour CC.green
      :* getAlphaColour CC.yellow
      :* getAlphaColour CC.blue
      :* getAlphaColour CC.magenta
      :* getAlphaColour CC.cyan
      :* getAlphaColour CC.base2
      :* EmptyVec

    solarizedDark2 :: Vec N8 (AlphaColour Double)
    solarizedDark2 =
         getAlphaColour CC.base03
      :* getAlphaColour CC.orange
      :* getAlphaColour CC.base01
      :* getAlphaColour CC.base00
      :* getAlphaColour CC.base0
      :* getAlphaColour CC.violet
      :* getAlphaColour CC.base1
      :* getAlphaColour CC.base3
      :* EmptyVec

solarizedLight :: ColourConfig (AlphaColour Double)
solarizedLight =
  defaultColourConfig
    { foregroundColour = Set (getAlphaColour CC.base00)
    , backgroundColour = Set (getAlphaColour CC.base3)
    , palette = ExtendedPalette solarizedLight1 solarizedLight2
    }
  where
    solarizedLight1 :: Vec N8 (AlphaColour Double)
    solarizedLight1 =
         getAlphaColour CC.base02
      :* getAlphaColour CC.red
      :* getAlphaColour CC.green
      :* getAlphaColour CC.yellow
      :* getAlphaColour CC.blue
      :* getAlphaColour CC.magenta
      :* getAlphaColour CC.cyan
      :* getAlphaColour CC.base2
      :* EmptyVec

    solarizedLight2 :: Vec N8 (AlphaColour Double)
    solarizedLight2 =
         getAlphaColour CC.base03
      :* getAlphaColour CC.orange
      :* getAlphaColour CC.base01
      :* getAlphaColour CC.base00
      :* getAlphaColour CC.base0
      :* getAlphaColour CC.violet
      :* getAlphaColour CC.base1
      :* getAlphaColour CC.base3
      :* EmptyVec

fontConf :: FontConfig
fontConf =
  defaultFontConfig
    { fontFamily = pack $ name C.font
    , fontSize   = FontSizePoints (size C.font)
    }

main :: IO ()
main = do
  myColourExt <- createColourExtension solarizedDark

  let newTMConfig = addColourExtension myTMConfig myColourExt

  start newTMConfig
