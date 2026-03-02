{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.List (intercalate)
import Text.Printf (printf)

data DimensionReport = DimensionReport
  { dim :: Int
  , domainDegree :: Int
  , codomainDegree :: Int
  , hasNativeEndomorphism :: Bool
  , hasSelfDualProjectors :: Bool
  , generatedSchemas :: [String]
  }

mkReport :: Int -> DimensionReport
mkReport d =
  let k = 2
      cod = d - k
      endo = cod == k
      projectors = endo
      schemas =
        baseSchemas d
          ++ if endo
                then [ "hodge-endomorphism:Omega^2->Omega^2"
                     , "projector:P+ = (Id + *)/2"
                     , "projector:P- = (Id - *)/2"
                     , "decomposition:Omega^2 = Omega^2_+ ⊕ Omega^2_-"
                     ]
                else []
   in DimensionReport d k cod endo projectors schemas

baseSchemas :: Int -> [String]
baseSchemas d =
  [ "forms:Omega^0..Omega^" <> show d
  , "hodge:*:Omega^k->Omega^(d-k)"
  , "coderivative:delta = ± * d *"
  ]

prettyReport :: DimensionReport -> String
prettyReport DimensionReport {..} =
  intercalate
    "\n"
    [ printf "d=%d" dim
    , printf "  Hodge on 2-forms: * : Omega^%d -> Omega^%d" domainDegree codomainDegree
    , printf "  Native endomorphism (Omega^2 -> Omega^2): %s" (yn hasNativeEndomorphism)
    , printf "  Self/anti-self-dual projectors available: %s" (yn hasSelfDualProjectors)
    , "  Schemas:"
    ]
      <> "\n"
      <> unlines (map ("    - " <>) generatedSchemas)

yn :: Bool -> String
yn True = "YES"
yn False = "NO"

verifyUnique4D :: [DimensionReport] -> Either String ()
verifyUnique4D rs =
  case [dim r | r <- rs, hasNativeEndomorphism r && hasSelfDualProjectors r] of
    [4] -> Right ()
    xs -> Left ("Expected only d=4 to have native Ω^2→Ω^2 with projectors; got " <> show xs)

main :: IO ()
main = do
  let dims = [2 .. 10]
      reports = map mkReport dims
  putStrLn "=== Artifact C: Hodge Endomorphism Scan ==="
  mapM_ (putStrLn . prettyReport) reports
  case verifyUnique4D reports of
    Right () -> do
      putStrLn "VERDICT: UNIQUE-4D-CONFIRMED"
      putStrLn "Only d=4 yields native Ω^2→Ω^2 Hodge endomorphism with self/anti-self-dual projectors."
    Left err -> do
      putStrLn "VERDICT: FAILED"
      putStrLn err
      error "Artifact C verification failed"
