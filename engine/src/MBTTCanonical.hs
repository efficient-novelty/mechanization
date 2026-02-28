{-# LANGUAGE BangPatterns #-}

-- | Phase 2 kickoff: canonical keys and normalization helpers for MBTT ASTs.
--
-- This module provides deterministic canonicalization primitives that can be
-- threaded into candidate deduplication/quotienting in the search pipeline.
module MBTTCanonical
  ( CanonKey(..)
  , canonicalizeExpr
  , canonicalizeSpec
  , canonicalKeyExpr
  , canonicalKeySpec
  ) where

import Data.Bits (xor)
import Data.Char (toLower)
import Data.List (foldl')
import Data.Word (Word32)
import Kolmogorov (MBTTExpr(..))
import Numeric (showHex)

-- | Stable key for canonicalized MBTT structures.
newtype CanonKey = CanonKey { unCanonKey :: String }
  deriving (Show, Eq, Ord)

-- | Canonicalize an MBTT expression.
--
-- For the Phase-2 kickoff, canonicalization is structural and idempotent:
-- recursively normalize all children while preserving constructor order.
canonicalizeExpr :: MBTTExpr -> MBTTExpr
canonicalizeExpr expr = case expr of
  App f x        -> App (canonicalizeExpr f) (canonicalizeExpr x)
  Lam e          -> Lam (canonicalizeExpr e)
  Pi a b         -> Pi (canonicalizeExpr a) (canonicalizeExpr b)
  Sigma a b      -> Sigma (canonicalizeExpr a) (canonicalizeExpr b)
  Id a x y       -> Id (canonicalizeExpr a) (canonicalizeExpr x) (canonicalizeExpr y)
  Refl e         -> Refl (canonicalizeExpr e)
  Susp e         -> Susp (canonicalizeExpr e)
  Trunc e        -> Trunc (canonicalizeExpr e)
  Flat e         -> Flat (canonicalizeExpr e)
  Sharp e        -> Sharp (canonicalizeExpr e)
  Disc e         -> Disc (canonicalizeExpr e)
  Shape e        -> Shape (canonicalizeExpr e)
  Next e         -> Next (canonicalizeExpr e)
  Eventually e   -> Eventually (canonicalizeExpr e)
  _              -> expr

-- | Canonicalize a specification-level list of expressions.
canonicalizeSpec :: [MBTTExpr] -> [MBTTExpr]
canonicalizeSpec = map canonicalizeExpr

-- | Compute a stable canonical key for an expression.
canonicalKeyExpr :: MBTTExpr -> CanonKey
canonicalKeyExpr = CanonKey . fnv1aHex . encodeExpr . canonicalizeExpr

-- | Compute a stable canonical key for a specification.
canonicalKeySpec :: [MBTTExpr] -> CanonKey
canonicalKeySpec = CanonKey . fnv1aHex . concatMap (encodeExpr . canonicalizeExpr)

encodeExpr :: MBTTExpr -> String
encodeExpr expr = case expr of
  App f x      -> "A(" ++ encodeExpr f ++ ")(" ++ encodeExpr x ++ ")"
  Lam e        -> "L(" ++ encodeExpr e ++ ")"
  Pi a b       -> "P(" ++ encodeExpr a ++ ")(" ++ encodeExpr b ++ ")"
  Sigma a b    -> "S(" ++ encodeExpr a ++ ")(" ++ encodeExpr b ++ ")"
  Univ         -> "U"
  Var i        -> "V" ++ show i
  Lib i        -> "B" ++ show i
  Id a x y     -> "I(" ++ encodeExpr a ++ ")(" ++ encodeExpr x ++ ")(" ++ encodeExpr y ++ ")"
  Refl e       -> "R(" ++ encodeExpr e ++ ")"
  Susp e       -> "Q(" ++ encodeExpr e ++ ")"
  Trunc e      -> "T(" ++ encodeExpr e ++ ")"
  PathCon d    -> "C" ++ show d
  Flat e       -> "F(" ++ encodeExpr e ++ ")"
  Sharp e      -> "H(" ++ encodeExpr e ++ ")"
  Disc e       -> "D(" ++ encodeExpr e ++ ")"
  Shape e      -> "G(" ++ encodeExpr e ++ ")"
  Next e       -> "N(" ++ encodeExpr e ++ ")"
  Eventually e -> "E(" ++ encodeExpr e ++ ")"

fnv1aHex :: String -> String
fnv1aHex input = pad8 . map toLower $ showHex digest ""
  where
    digest :: Word32
    digest = foldl' step offsetBasis input

    offsetBasis :: Word32
    offsetBasis = 2166136261

    prime :: Word32
    prime = 16777619

    step :: Word32 -> Char -> Word32
    step !h c = (h `xor` fromIntegral (fromEnum c)) * prime

    pad8 s = replicate (max 0 (8 - length s)) '0' ++ s
