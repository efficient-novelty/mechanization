-- | PEN Information-Theoretic Engine
--
-- Main module for computing Kolmogorov κ and Shannon ν
-- for the Genesis sequence.

module Main where

import Types
import Inhabitation
import Enumerate
import KappaNu
import ProofRank
import Data.List (sortOn)
import Text.Printf
import System.Directory (doesFileExist)

-- ============================================
-- Main Entry Point
-- ============================================

main :: IO ()
main = do
  putStrLn "PEN Information-Theoretic Engine"
  putStrLn "================================="
  putStrLn ""

  -- Run tests for R1-R10
  putStrLn "Testing Genesis Sequence (R1-R10)"
  putStrLn "----------------------------------"
  putStrLn ""

  let maxK = 4  -- Complexity horizon

  putStrLn $ "Complexity horizon k = " ++ show maxK
  putStrLn ""

  putStrLn "| n  | Structure   | κ_comp | κ_paper | ν_comp | ν_paper | Match? |"
  putStrLn "|----|-------------|--------|---------|--------|---------|--------|"

  mapM_ (printStep maxK) [1..10]

  putStrLn ""
  putStrLn "Analysis:"
  putStrLn "---------"

  -- Show newly inhabited types for S¹
  putStrLn ""
  putStrLn "Newly inhabited types when adding S¹ (step 5):"
  let lib4 = buildLibrary 4
  let s1Entry = genesisEntry 5
  let newTypes = getNewlyInhabited s1Entry lib4 maxK
  mapM_ (putStrLn . ("  " ++) . prettyTypeExpr) newTypes

  putStrLn ""
  putStrLn "Type enumeration statistics:"
  let lib5 = buildLibrary 5
  let types5 = allTypes lib5 maxK
  putStrLn $ "  Total types at k=" ++ show maxK ++ " with library(5): " ++ show (length types5)
  let byLevel = countByComplexity types5
  mapM_ (\(k, c) -> putStrLn $ "    Complexity " ++ show k ++ ": " ++ show c ++ " types") byLevel

  putStrLn ""
  putStrLn "Proof-rank validation (depth-2):"
  putStrLn "--------------------------------"
  reportProofRank "Pi/Sigma" 4 3 (5, 6)
  putStrLn ""
  reportProofRank "S1" 5 4 (7, 7)

  putStrLn ""
  putStrLn "Proof-rank (depth-2) using Agda manifest:"
  putStrLn "-----------------------------------------"
  let manifestPath = "agda/library_manifest.json"
  manifestExists <- doesFileExist manifestPath
  if not manifestExists
    then putStrLn $ "Manifest not found at " ++ manifestPath
    else do
      manifestResult <- loadManifest manifestPath
      case manifestResult of
        Left err -> putStrLn $ "Failed to parse manifest: " ++ err
        Right lib -> case splitLibraryAt "S1" lib of
          Nothing -> putStrLn "Manifest does not include S1 entry."
          Just (beforeS1, s1EntryFromManifest) -> do
            let (manifestRank, manifestClusters) = proofRank s1EntryFromManifest beforeS1 2
            putStrLn $ "Depth-2 proof-rank ν(S1 | manifest) = " ++ show manifestRank
            putStrLn "Clusters:"
            mapM_ (putStrLn . formatCluster) (zip [1 :: Int ..] manifestClusters)

-- | Print comparison for a single step
printStep :: Int -> Int -> IO ()
printStep maxK n = do
  let (ck, cv, pk, pv) = compareStep n maxK
      name = structureName n
      match = if cv == pv then "YES" else "no"
  printf "| %2d | %-11s | %6d | %7d | %6d | %7d | %-6s |\n"
         n name ck pk cv pv match

-- | Human-readable structure names
structureName :: Int -> String
structureName 1 = "Universe"
structureName 2 = "Unit"
structureName 3 = "Witness"
structureName 4 = "Pi/Sigma"
structureName 5 = "S1"
structureName 6 = "PropTrunc"
structureName 7 = "S2"
structureName 8 = "S3"
structureName 9 = "Hopf"
structureName 10 = "Lie"
structureName _ = "???"

formatClusterSummary :: (Int, [TypeExpr]) -> String
formatClusterSummary (idx, ts) =
  let sorted = sortOn prettyTypeExpr ts
      representative = case sorted of
        [] -> "<empty>"
        (t:_) -> prettyTypeExpr t
  in "  [" ++ show idx ++ "] size=" ++ show (length ts) ++ " rep=" ++ representative

reportProofRank :: String -> Int -> Int -> (Int, Int) -> IO ()
reportProofRank label newStep libStep (targetMin, targetMax) = do
  let entry = genesisEntry newStep
      lib = buildLibrary libStep
      (rank, clusters) = proofRank entry lib 2
      targetNote
        | rank < targetMin = "below target"
        | rank > targetMax = "above target"
        | otherwise = "within target"
  putStrLn $ label ++ " depth-2 ν = " ++ show rank
  putStrLn $ "Target range: " ++ show targetMin ++ "–" ++ show targetMax ++ " (" ++ targetNote ++ ")"
  putStrLn "Clusters (size + representative):"
  mapM_ (putStrLn . formatClusterSummary) (zip [1 :: Int ..] clusters)

splitLibraryAt :: String -> Library -> Maybe (Library, LibraryEntry)
splitLibraryAt _ [] = Nothing
splitLibraryAt target (entry:rest)
  | leName entry == target = Just ([], entry)
  | otherwise = do
      (prefix, found) <- splitLibraryAt target rest
      pure (entry : prefix, found)

-- ============================================
-- Interactive Testing
-- ============================================

-- | Test inhabitation for a specific type
testInhab :: TypeExpr -> Library -> IO ()
testInhab t lib = do
  putStrLn $ "Testing: " ++ prettyTypeExpr t
  case checkInhab t lib of
    Inhabited w -> putStrLn $ "  Inhabited: " ++ show w
    NotInhabited r -> putStrLn $ "  Not inhabited: " ++ show r
    Unknown -> putStrLn "  Unknown"

-- | Test a series of types
testSeries :: IO ()
testSeries = do
  let lib = buildLibrary 5  -- Library with S¹

  putStrLn "Testing inhabitation with library(5):"
  putStrLn ""

  -- Basic types
  testInhab TUnit lib
  testInhab TVoid lib
  testInhab (TRef "S1") lib

  -- Function types
  testInhab (TArrow TUnit TUnit) lib
  testInhab (TArrow (TRef "S1") TUnit) lib

  -- Loop space of S¹
  testInhab (TOmega (TRef "S1")) lib

  -- Products
  testInhab (TProd (TRef "S1") (TRef "S1")) lib

  -- Self-identity
  testInhab (TSelfId (TRef "S1")) lib

  -- Suspension
  testInhab (TSusp (TRef "S1")) lib

-- | Enumerate and display types
showEnumeration :: Int -> Int -> IO ()
showEnumeration libSize maxK = do
  let lib = buildLibrary libSize
  let types = allTypes lib maxK
  putStrLn $ "Types up to complexity " ++ show maxK ++ " with library(" ++ show libSize ++ "):"
  mapM_ (putStrLn . ("  " ++) . prettyTypeExpr) types
