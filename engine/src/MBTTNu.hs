{-# LANGUAGE BangPatterns #-}

-- | MBTTNu — Native ν extraction entrypoint from anonymous MBTT terms.
--
-- Phase-3 native-ν adapter:
-- keeps the current StructuralNu computation backend while exposing a stable
-- API and explainability trace contract that can be consumed by evaluators and
-- CI evidence tooling.
module MBTTNu
  ( NativeNuResult(..)
  , computeNativeNu
  ) where

import Kolmogorov (MBTTExpr(..))
import Telescope (Telescope, teleEntries, teType)
import Types (Library)
import StructuralNu (StructuralNuResult(..), structuralNu)

-- | Native ν decomposition computed from MBTT structure.
--
-- `nnTrace` contains stable key/value summary lines plus node-level provenance
-- records (`node=...|ctor=...`) to support Phase-3 explainability workflows.
data NativeNuResult = NativeNuResult
  { nnNuG   :: !Int
  , nnNuH   :: !Int
  , nnNuC   :: !Int
  , nnTotal :: !Int
  , nnTrace :: ![String]
  } deriving (Show, Eq)

-- | Compute native ν from the MBTT telescope AST.
computeNativeNu :: Telescope -> Library -> [(Int, Int)] -> NativeNuResult
computeNativeNu tele lib nuHistory =
  let sr = structuralNu tele lib nuHistory
      nodeTrace = concatMap traceEntry (zip [(1 :: Int)..] (map teType (teleEntries tele)))
      trace =
        [ "source=structural-ast"
        , "nu_g=" ++ show (snNuG sr)
        , "nu_h=" ++ show (snNuH sr)
        , "nu_c=" ++ show (snNuC sr)
        , "bonus_distributive=" ++ show (snDistLaw sr)
        , "bonus_universe_poly=" ++ show (snUnivPoly sr)
        , "bonus_infinitesimal_shift=" ++ show (snInfShift sr)
        , "nu_total=" ++ show (snTotal sr)
        , "node_trace_count=" ++ show (length nodeTrace)
        ] ++ nodeTrace
  in NativeNuResult
      { nnNuG = snNuG sr
      , nnNuH = snNuH sr
      , nnNuC = snNuC sr
      , nnTotal = snTotal sr
      , nnTrace = trace
      }

-- | Emit node-level provenance records for an entry expression.
traceEntry :: (Int, MBTTExpr) -> [String]
traceEntry (entryIx, expr) = go ("entry" ++ show entryIx) expr
  where
    go path e =
      let here = ["node=" ++ path ++ "|ctor=" ++ ctorName e]
      in here ++ concatChildren path e

    concatChildren path e =
      case e of
        Var _              -> []
        Lib _              -> []
        Univ               -> []
        App f x            -> go (path ++ "/0") f ++ go (path ++ "/1") x
        Lam b              -> go (path ++ "/0") b
        Pi a b             -> go (path ++ "/0") a ++ go (path ++ "/1") b
        Sigma a b          -> go (path ++ "/0") a ++ go (path ++ "/1") b
        Id a x y           -> go (path ++ "/0") a ++ go (path ++ "/1") x ++ go (path ++ "/2") y
        Refl x             -> go (path ++ "/0") x
        Susp x             -> go (path ++ "/0") x
        Trunc x            -> go (path ++ "/0") x
        PathCon _          -> []
        Flat x             -> go (path ++ "/0") x
        Sharp x            -> go (path ++ "/0") x
        Disc x             -> go (path ++ "/0") x
        Shape x            -> go (path ++ "/0") x
        Next x             -> go (path ++ "/0") x
        Eventually x       -> go (path ++ "/0") x

-- | Constructor tag used in node-level provenance records.
ctorName :: MBTTExpr -> String
ctorName e = case e of
  Var _          -> "Var"
  Lib _          -> "Lib"
  Univ           -> "Univ"
  App _ _        -> "App"
  Lam _          -> "Lam"
  Pi _ _         -> "Pi"
  Sigma _ _      -> "Sigma"
  Id _ _ _       -> "Id"
  Refl _         -> "Refl"
  Susp _         -> "Susp"
  Trunc _        -> "Trunc"
  PathCon _      -> "PathCon"
  Flat _         -> "Flat"
  Sharp _        -> "Sharp"
  Disc _         -> "Disc"
  Shape _        -> "Shape"
  Next _         -> "Next"
  Eventually _   -> "Eventually"
