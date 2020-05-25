module Color
( Color (..)
, showWebColor
, showCColor
) where

import GHC.Word
import Numeric (showHex)

data Color =
  Color {
    r :: Word8
  , g :: Word8
  , b :: Word8
  }
  deriving (Eq)

instance Show Color where
  showsPrec _ (Color r' g' b') = pad r' . showHex r' . pad g' .showHex g' . pad b' . showHex b'
    where
      pad c
        | c < 16    = showString "0"
        | otherwise = showString ""

showWebColor :: Color -> String
showWebColor = ('#' :) . show

showCColor :: Color -> String
showCColor = ("0x" ++) . show
