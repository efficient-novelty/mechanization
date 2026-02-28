{-# LANGUAGE BangPatterns #-}

-- | MBTTDecode — reporting-only semantic decoding for anonymous MBTT winners.
--
-- Phase-5 API boundary: maps anonymous/canonical identifiers to optional
-- semantic descriptors with confidence + ambiguity metadata. This module must
-- remain non-interfering: it is for post-hoc reporting only.
module MBTTDecode
  ( DecodeResult(..)
  , decodeCanonicalName
  , decodeCanonicalNameWithKey
  ) where

-- | Deterministic decode output schema for reporting surfaces.
data DecodeResult = DecodeResult
  { drCanonicalName :: !String
  , drCanonicalKey  :: !(Maybe String)
  , drDecodedLabel  :: !(Maybe String)
  , drConfidence    :: !Double
  , drAmbiguity     :: ![String]
  , drNonInterfering :: !Bool
  } deriving (Show, Eq)

-- | Decode from canonical name only.
decodeCanonicalName :: String -> DecodeResult
decodeCanonicalName name = decodeCanonicalNameWithKey name Nothing

-- | Decode from canonical name + optional canonical key.
--
-- The canonical key is carried through for reporting joins but does not alter
-- selection behavior.
decodeCanonicalNameWithKey :: String -> Maybe String -> DecodeResult
decodeCanonicalNameWithKey name mKey =
  case name of
    "Universe"    -> mk (Just "Universe U₀") 1.00 []
    "Unit"        -> mk (Just "Unit type 1") 0.99 []
    "Witness"     -> mk (Just "Unit witness ★") 0.98 []
    "Pi"          -> mk (Just "Dependent function space") 0.90 ["Dependent product"]
    "S1"          -> mk (Just "Circle HIT S¹") 0.96 []
    "Trunc"       -> mk (Just "Propositional truncation") 0.93 ["(-1)-truncation"]
    "S2"          -> mk (Just "2-sphere HIT S²") 0.95 []
    "S3"          -> mk (Just "3-sphere HIT S³") 0.95 []
    "Hopf"        -> mk (Just "Hopf fibration") 0.92 ["Principal S¹-bundle over S²"]
    "Cohesion"    -> mk (Just "Cohesive modality layer") 0.85 ["Modal context"]
    "Connections" -> mk (Just "Connection structure") 0.88 []
    "Curvature"   -> mk (Just "Curvature structure") 0.88 []
    "Metric"      -> mk (Just "Metric structure") 0.90 []
    "Hilbert"     -> mk (Just "Hilbert-space layer") 0.90 []
    "DCT"         -> mk (Just "Dynamical cohesive topos") 0.91 ["Temporal cohesive synthesis"]
    "candidate"   -> mk Nothing 0.0 []
    _              -> mk Nothing 0.0 []
  where
    mk lbl conf amb = DecodeResult
      { drCanonicalName = name
      , drCanonicalKey = mKey
      , drDecodedLabel = lbl
      , drConfidence = conf
      , drAmbiguity = amb
      , drNonInterfering = True
      }
