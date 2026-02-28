{-# LANGUAGE BangPatterns #-}

-- | MBTTNu — Native ν extraction entrypoint from anonymous MBTT terms.
--
-- Phase-3 kickoff adapter:
-- keeps the existing StructuralNu implementation as the scoring backend,
-- while centralizing a native-MBTT-facing API (`computeNativeNu`) and an
-- explainability trace shape that can be extended with AST-node-level evidence.
module MBTTNu
  ( NativeNuResult(..)
  , computeNativeNu
  ) where

import Telescope (Telescope)
import Types (Library)
import StructuralNu (StructuralNuResult(..), structuralNu)

-- | Native ν decomposition computed from MBTT structure.
--
-- `nnTrace` is intentionally lightweight for now (Phase-3 kickoff): it gives
-- a stable, machine-readable summary of the decomposition and multiplier
-- contributions. Later Phase-3 iterations can append node-level provenance.
data NativeNuResult = NativeNuResult
  { nnNuG   :: !Int
  , nnNuH   :: !Int
  , nnNuC   :: !Int
  , nnTotal :: !Int
  , nnTrace :: ![String]
  } deriving (Show, Eq)

-- | Compute native ν from the MBTT telescope AST.
--
-- This function is the Phase-3 API boundary used by evaluators.
computeNativeNu :: Telescope -> Library -> [(Int, Int)] -> NativeNuResult
computeNativeNu tele lib nuHistory =
  let sr = structuralNu tele lib nuHistory
      trace =
        [ "source=structural-ast"
        , "nu_g=" ++ show (snNuG sr)
        , "nu_h=" ++ show (snNuH sr)
        , "nu_c=" ++ show (snNuC sr)
        , "bonus_distributive=" ++ show (snDistLaw sr)
        , "bonus_universe_poly=" ++ show (snUnivPoly sr)
        , "bonus_infinitesimal_shift=" ++ show (snInfShift sr)
        , "nu_total=" ++ show (snTotal sr)
        ]
  in NativeNuResult
      { nnNuG = snNuG sr
      , nnNuH = snNuH sr
      , nnNuC = snNuC sr
      , nnTotal = snTotal sr
      , nnTrace = trace
      }
