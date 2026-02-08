-- | PEN Information-Theoretic Engine
--
-- Main module: proof-rank validation and Shannon ν comparison
-- for the Genesis sequence.
--
-- NOTE: Kolmogorov κ (full program enumeration) is disabled in this run
-- because the search space explodes combinatorially beyond cost ~5.
-- The proof-rank validation is the primary deliverable.

module Main where

import Types
import Inhabitation
import Enumerate
import KappaNu
import ProofRank
import Capability (genesisDescriptor, computeNu, computedNuSimple, CapTrace(..))
import Manifest (loadManifest)
import Simulation (runSimulation, formatSimTable, defaultConfig, capabilityConfig, trCleared, trName)
import Synthesis (runSynthesis, formatSynthTable, formatSynthComparison, defaultSynthConfig)
import Data.List (sortOn, intercalate)
import qualified Data.Map.Strict as Map
import System.Directory (doesFileExist)

-- ============================================
-- Main Entry Point
-- ============================================

main :: IO ()
main = do
  putStrLn "PEN Information-Theoretic Engine v0.4"
  putStrLn "====================================="
  putStrLn ""

  -- Phase A: Type enumeration statistics
  putStrLn "Phase A: Type Enumeration Statistics"
  putStrLn "------------------------------------"
  mapM_ showEnumStats [1..7]

  -- Phase B: Shannon ν (raw newly-inhabited count at complexity ≤ 4)
  putStrLn ""
  putStrLn "Phase B: Shannon ν (raw newly-inhabited type count, k ≤ 4)"
  putStrLn "-----------------------------------------------------------"
  putStrLn "| n  | Structure   | ν_raw | ν_paper | Types |"
  putStrLn "|----|-------------|-------|---------|-------|"
  mapM_ printShannon [1..8]

  -- Phase C: Newly inhabited types for key steps (detailed)
  putStrLn ""
  putStrLn "Phase C: Detailed newly inhabited types"
  putStrLn "----------------------------------------"

  putStrLn ""
  putStrLn "--- Step 3: Witness (★) added to {U, 1} ---"
  showNewlyInhabited 3

  putStrLn ""
  putStrLn "--- Step 4: Π/Σ added to {U, 1, ★} ---"
  showNewlyInhabited 4

  putStrLn ""
  putStrLn "--- Step 5: S¹ added to {U, 1, ★, Π/Σ} ---"
  showNewlyInhabited 5

  putStrLn ""
  putStrLn "--- Step 6: PropTrunc added to {U, 1, ★, Π/Σ, S¹} ---"
  showNewlyInhabited 6

  putStrLn ""
  putStrLn "--- Step 7: S² added to {U, 1, ★, Π/Σ, S¹, Trunc} ---"
  showNewlyInhabited 7

  -- Phase D: Proof-rank validation (the key result)
  putStrLn ""
  putStrLn "Phase D: Proof-Rank Validation (depth-2 clustering)"
  putStrLn "==================================================="
  putStrLn ""
  putStrLn "This is the key validation target. Proof-rank ν counts"
  putStrLn "independent derivability clusters among newly inhabited"
  putStrLn "types at expression depth ≤ 2."
  putStrLn ""

  mapM_ (\(label, n, targetMin, targetMax) -> do
    reportProofRank label n (targetMin, targetMax)
    putStrLn ""
    ) [ ("Witness (★)",  3, 1, 2)
      , ("Pi/Sigma",     4, 4, 6)
      , ("S1 (Circle)",  5, 5, 7)
      , ("PropTrunc",    6, 6, 8)
      , ("S2 (Sphere)",  7, 5, 10)
      , ("S3",           8, 8, 18)
      ]

  -- Phase E: Manifest-based test
  putStrLn ""
  putStrLn "Phase E: Agda Manifest Proof-Rank"
  putStrLn "----------------------------------"
  let manifestPath = "agda/library_manifest.json"
  manifestExists <- doesFileExist manifestPath
  if not manifestExists
    then putStrLn $ "Manifest not found at " ++ manifestPath
    else do
      manifestResult <- loadManifest manifestPath
      case manifestResult of
        Left err -> putStrLn $ "Failed to parse manifest: " ++ err
        Right lib -> do
          putStrLn $ "Manifest loaded: " ++ show (length lib) ++ " entries"
          putStrLn $ "Entries: " ++ intercalate ", " (map leName lib)
          case splitLibraryAt "S1" lib of
            Nothing -> putStrLn "Manifest does not include S1 entry."
            Just (beforeS1, s1Entry) -> do
              putStrLn ""
              putStrLn $ "Testing S1 against library: " ++ intercalate ", " (map leName beforeS1)
              let (rank, clusters) = proofRank s1Entry beforeS1 2
              putStrLn $ "Depth-2 proof-rank ν(S1 | manifest) = " ++ show rank
              putStrLn "Clusters:"
              mapM_ (putStrLn . formatCluster) (zip [1 :: Int ..] clusters)

  -- Phase F: K-Based Novelty (compression improvement)
  putStrLn ""
  putStrLn "Phase F: K-Based Novelty (compression improvement, H=5)"
  putStrLn "======================================================="
  putStrLn ""
  putStrLn "K-based novelty counts types whose Kolmogorov complexity drops"
  putStrLn "after adding X. This captures Pi/Sigma and PropTrunc contributions"
  putStrLn "that inhabitation-based ν misses."
  putStrLn ""

  mapM_ (\(label, n) -> do
    reportKNovelty label n
    putStrLn ""
    ) [ ("Witness (★)",  3)
      , ("Pi/Sigma",     4)
      , ("S1 (Circle)",  5)
      , ("PropTrunc",    6)
      , ("S2 (Sphere)",  7)
      , ("S3",           8)
      ]

  -- Phase G: PEN Axiom Simulation
  putStrLn ""
  putStrLn "Phase G: PEN Axiom Simulation"
  putStrLn "=============================="
  putStrLn ""
  putStrLn "Tick-by-tick simulation of the five PEN axioms."
  putStrLn "Paper mode: uses paperKappa/paperNu reference values."
  putStrLn ""

  simResults <- runSimulation defaultConfig
  putStrLn (formatSimTable simResults)

  let nCleared = length (filter trCleared simResults)
      nTotal   = length simResults
  putStrLn $ show nCleared ++ "/" ++ show nTotal ++ " candidates realized (out of 16 Genesis structures)."

  -- Phase H: Capability Engine Validation
  putStrLn ""
  putStrLn "Phase H: Capability Engine Validation"
  putStrLn "======================================"
  putStrLn ""
  putStrLn "Per-structure capability breakdown: computed ν vs paper ν."
  putStrLn ""
  putStrLn " n  | Structure      | ν_comp | ν_paper | Match | Rule breakdown"
  putStrLn "----|----------------|--------|---------|-------|-------------------------------------------"

  mapM_ (\n -> do
    let sd = genesisDescriptor n
        priors = [genesisDescriptor i | i <- [1..n-1]]
        (nuComp, traces) = computeNu sd priors
        nuPaper = paperNu n
        match = if nuComp == nuPaper then "YES" else " NO"
        activeTraces = filter (\t -> ctCount t > 0) traces
        traceStr = intercalate ", " [ctRule t ++ "(" ++ show (ctCount t) ++ ")"
                                    | t <- activeTraces]
    putStrLn $ padR 3 (show n) ++ " | "
            ++ padR 14 (structureName n) ++ " | "
            ++ padR 6 (show nuComp) ++ " | "
            ++ padR 7 (show nuPaper) ++ " | "
            ++ padR 5 match ++ " | "
            ++ traceStr
    ) [1..16]

  let allMatch = all (\n -> computedNuSimple n == paperNu n) [1..16]
  putStrLn ""
  if allMatch
    then putStrLn "ALL 16 structures: ν_computed == ν_paper (MATCH)"
    else putStrLn "WARNING: Some structures do NOT match!"

  -- Phase I: Capability-Mode Simulation
  putStrLn ""
  putStrLn "Phase I: Capability-Mode Simulation"
  putStrLn "===================================="
  putStrLn ""
  putStrLn "Tick-by-tick simulation using capability-computed ν values."
  putStrLn ""

  capResults <- runSimulation capabilityConfig
  putStrLn (formatSimTable capResults)

  let nClearedCap = length (filter trCleared capResults)
      nTotalCap   = length capResults
  putStrLn $ show nClearedCap ++ "/" ++ show nTotalCap ++ " candidates realized (CapabilityMode)."

  -- Compare Phase G vs Phase I
  putStrLn ""
  let paperNames = map (\r -> trName r) simResults
      capNames   = map (\r -> trName r) capResults
      identical  = paperNames == capNames
  if identical
    then putStrLn "Phase G vs Phase I: IDENTICAL realization sequences."
    else do
      putStrLn "Phase G vs Phase I: DIFFERENT realization sequences!"
      putStrLn $ "  Paper mode:      " ++ intercalate ", " paperNames
      putStrLn $ "  Capability mode: " ++ intercalate ", " capNames

  -- Phase J: Synthesis Mode (Object Construction from Primitives)
  putStrLn ""
  putStrLn "Phase J: Synthesis Mode (Object Construction from Primitives)"
  putStrLn "============================================================="
  putStrLn ""
  putStrLn "Discovering Genesis structures via genuine search."
  putStrLn "Candidates are generated, evaluated (genuine nu + kappa),"
  putStrLn "and selected by the PEN axioms — not replayed from a table."
  putStrLn ""

  synthResults <- runSynthesis defaultSynthConfig
  putStrLn (formatSynthTable synthResults)
  putStrLn ""
  putStrLn (formatSynthComparison synthResults)

  putStrLn ""
  putStrLn "=== Engine run complete ==="

-- ============================================
-- Helpers
-- ============================================

showEnumStats :: Int -> IO ()
showEnumStats n = do
  let lib = buildLibrary n
      types3 = allTypes lib 3
      types4 = allTypes lib 4
  putStrLn $ "Library(" ++ show n ++ "): |types_k≤3| = " ++ show (length types3)
                       ++ ", |types_k≤4| = " ++ show (length types4)

printShannon :: Int -> IO ()
printShannon n = do
  let lib = buildLibrary (n - 1)
      entry = genesisEntry n
      name = structureName n
      maxK = 4
      newTypes = getNewlyInhabited entry lib maxK
      rawNu = length newTypes
      pNu = paperNu n
  putStrLn $ "| " ++ padR 2 (show n) ++ " | " ++ padR 11 name
           ++ " | " ++ padR 5 (show rawNu)
           ++ " | " ++ padR 7 (show pNu)
           ++ " | " ++ intercalate ", " (map prettyTypeExpr (take 8 newTypes))
           ++ (if length newTypes > 8 then ", ..." else "")
           ++ " |"

showNewlyInhabited :: Int -> IO ()
showNewlyInhabited n = do
  let lib = buildLibrary (n - 1)
      entry = genesisEntry n
      maxK = 4
      newTypes = getNewlyInhabited entry lib maxK
  putStrLn $ "  Count: " ++ show (length newTypes)
  mapM_ (\t -> do
    let inhab = checkInhab t (entry : lib)
    putStrLn $ "  " ++ prettyTypeExpr t ++ "  [" ++ showInhabBrief inhab ++ "]"
    ) newTypes

reportProofRank :: String -> Int -> (Int, Int) -> IO ()
reportProofRank label n (targetMin, targetMax) = do
  let entry = genesisEntry n
      lib = buildLibrary (n - 1)
      windowAts = windowAtoms entry lib
      fullLib = entry : lib
      maxDepth = 1  -- Depth-1: single type former applied to atoms
      allWindowTypes = enumWindowBounded windowAts fullLib maxDepth
      newTypes = newlyInhabitedWindow entry lib maxDepth
      (rank, clusters) = proofRank entry lib maxDepth
      targetNote
        | rank < targetMin = " << BELOW TARGET"
        | rank > targetMax = " >> ABOVE TARGET"
        | otherwise = " (within target)"
  putStrLn $ label ++ ":"
  putStrLn $ "  Window atoms: " ++ intercalate ", " (map prettyTypeExpr windowAts)
  putStrLn $ "  Types at depth ≤ " ++ show maxDepth ++ ": " ++ show (length allWindowTypes)
  putStrLn $ "  Newly inhabited: " ++ show (length newTypes)
  putStrLn $ "  Proof-rank ν = " ++ show rank
           ++ " (target: " ++ show targetMin ++ "–" ++ show targetMax ++ ")"
           ++ targetNote
  putStrLn "  Clusters:"
  mapM_ (putStrLn . ("  " ++) . formatCluster) (zip [1 :: Int ..] clusters)

formatCluster :: (Int, [TypeExpr]) -> String
formatCluster (idx, ts) =
  let sorted = sortOn prettyTypeExpr ts
      names = map prettyTypeExpr sorted
  in "[" ++ show idx ++ "] size=" ++ show (length ts)
     ++ ": {" ++ intercalate ", " (take 5 names)
     ++ (if length names > 5 then ", ..." else "") ++ "}"

reportKNovelty :: String -> Int -> IO ()
reportKNovelty label n = do
  let entry = genesisEntry n
      lib = buildLibrary (n - 1)
      fullLib = entry : lib
      horizon = 5
      costBefore = buildCostMap lib horizon
      costAfter  = buildCostMap fullLib horizon
      progsBefore = Map.size costBefore
      progsAfter  = Map.size costAfter
      (rank, clusters) = kNovelty entry lib horizon
      pNu = paperNu n
  putStrLn $ label ++ ":"
  putStrLn $ "  Distinct reachable types (before): " ++ show progsBefore
  putStrLn $ "  Distinct reachable types (after):  " ++ show progsAfter
  putStrLn $ "  ν_K = " ++ show rank ++ "  (ν_paper = " ++ show pNu ++ ")"
  putStrLn "  Clusters:"
  mapM_ (putStrLn . ("  " ++) . formatCluster) (zip [1 :: Int ..] clusters)

splitLibraryAt :: String -> Library -> Maybe (Library, LibraryEntry)
splitLibraryAt _ [] = Nothing
splitLibraryAt target (entry:rest)
  | leName entry == target = Just ([], entry)
  | otherwise = do
      (prefix, found) <- splitLibraryAt target rest
      pure (entry : prefix, found)

showInhabBrief :: InhabResult -> String
showInhabBrief (Inhabited w) = "inh: " ++ showWitnessBrief w
showInhabBrief (NotInhabited _) = "not"
showInhabBrief Unknown = "?"

showWitnessBrief :: Witness -> String
showWitnessBrief WUnit = "★"
showWitnessBrief (WConstructor s) = s
showWitnessBrief WRefl = "refl"
showWitnessBrief (WConst _) = "const"
showWitnessBrief (WPair _ _) = "pair"
showWitnessBrief (WInl _) = "inl"
showWitnessBrief (WInr _) = "inr"
showWitnessBrief (WLoop s) = "loop(" ++ s ++ ")"
showWitnessBrief WNorth = "north"
showWitnessBrief WVacuous = "vacuous"
showWitnessBrief (WPiIntro _) = "λ"
showWitnessBrief (WSigmaIntro _ _) = "sigma"

structureName :: Int -> String
structureName 1  = "Universe"
structureName 2  = "Unit"
structureName 3  = "Witness"
structureName 4  = "Pi/Sigma"
structureName 5  = "S1"
structureName 6  = "PropTrunc"
structureName 7  = "S2"
structureName 8  = "S3"
structureName 9  = "Hopf"
structureName 10 = "Lie"
structureName 11 = "Cohesion"
structureName 12 = "Connections"
structureName 13 = "Curvature"
structureName 14 = "Metric"
structureName 15 = "Hilbert"
structureName 16 = "DCT"
structureName _  = "???"

padR :: Int -> String -> String
padR n s = s ++ replicate (max 0 (n - length s)) ' '
