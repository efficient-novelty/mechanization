{-# LANGUAGE DeriveGeneric #-}

-- | JSON manifest loader for Agda library inventory.
--
-- Provides a simple bridge from an on-disk manifest to the engine's
-- LibraryEntry list so the proof-rank prototype can operate on the
-- same inventory as the Agda code.

module Manifest
  ( Manifest(..)
  , loadManifest
  ) where

import Data.Aeson (FromJSON (..), (.:), (.:?), withObject)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import GHC.Generics (Generic)

import Types (Library, LibraryEntry (..))

data Manifest = Manifest
  { manifestLibrary :: [LibraryEntry]
  } deriving (Eq, Show, Generic)

instance FromJSON Manifest where
  parseJSON = withObject "Manifest" $ \obj -> do
    library <- obj .: "library"
    pure (Manifest library)

instance FromJSON LibraryEntry where
  parseJSON = withObject "LibraryEntry" $ \obj -> do
    name <- obj .: "name"
    constructors <- obj .:? "constructors" Aeson..!= 0
    pathDims <- obj .:? "pathDims" Aeson..!= []
    hasLoop <- obj .:? "hasLoop" Aeson..!= False
    isTruncated <- obj .:? "truncation"
    pure (LibraryEntry name constructors pathDims hasLoop isTruncated)

loadManifest :: FilePath -> IO (Either String Library)
loadManifest path = do
  contents <- BL.readFile path
  pure (fmap manifestLibrary (Aeson.eitherDecode contents))
