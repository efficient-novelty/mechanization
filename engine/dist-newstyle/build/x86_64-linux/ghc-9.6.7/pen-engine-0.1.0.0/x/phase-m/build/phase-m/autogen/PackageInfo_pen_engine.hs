{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_pen_engine (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "pen_engine"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "PEN Information-Theoretic Engine"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
