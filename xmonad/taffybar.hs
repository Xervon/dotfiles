{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans.Reader
import Data.Unique
import Graphics.UI.GIGtkStrut
import System.Taffybar
import System.Taffybar.Context

myBarConfig :: Unique -> BarConfig
myBarConfig b = BarConfig
  { strutConfig   = defaultStrutConfig
                      { strutXPadding = 3
                      , strutYPadding = 3
                      }
  , widgetSpacing = 5
  , startWidgets  = []
  , centerWidgets = []
  , endWidgets    = []
  , barId         = b
  }

barConfigGetter :: Context -> TaffyIO [BarConfig]
barConfigGetter c = do
  barConfigs <- reader c (\c -> repeat newUnique >>= (\b -> [myBarConfig b]))
  return reader c barConfigs

main :: IO ()
main = do
  startTaffybar defaultTaffybarConfig
