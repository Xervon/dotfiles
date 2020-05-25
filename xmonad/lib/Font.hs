module Font
( Font (..)
, Style (..)
) where

import Data.List.Extra

data Style = Regular
           | Bold
           | Italic
           deriving (Show, Eq)

data Font = Font
  { name      :: String
  , pixelSize :: Int
  , style     :: Style
  , antialias :: Bool
  } deriving (Eq)

instance Show Font where
  showsPrec _ (Font n s st a) = showString $ "xft:" ++ n ++ ":pixelsize=" ++ (show s) ++ ":" ++ (lower $ show st) ++ ":antialias=" ++ (lower $ show a)
