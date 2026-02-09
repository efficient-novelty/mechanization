-- | Standalone runner for Phase M (Exact ν Oracle)
-- Run with: cabal run phase-m

module Main where

import Types
import KappaNu (genesisEntry, buildLibrary, paperNu)
import ExactNu (computeExactNuAtDepth)
import Cluster (proofRankNu)
import ProofRank (kNovelty)
import System.IO (hFlush, stdout)

flushLn :: String -> IO ()
flushLn s = putStrLn s >> hFlush stdout

main :: IO ()
main = do
  flushLn "Phase M: Exact ν Oracle (all library atoms, depths 1-3)"
  flushLn "========================================================"
  flushLn ""
  flushLn "Compares proof-rank ν (2-step window + latent bonus) against exact ν"
  flushLn "(all atoms, depths 1-3). Sanity: d1 schemas should match PR schemas."
  flushLn ""

  -- Part 1: Depths 1 and 2 for all 7 steps (fast)
  flushLn "--- Part 1: Depths 1 & 2 (steps 1-7) ---"
  flushLn ""
  flushLn " n  | Structure      | ν_paper | ν_PR | bonus | PR_sch | exact_d1 | exact_d2 | d1==PR?"
  flushLn "----|----------------|---------|------|-------|--------|----------|----------|--------"

  mapM_ (\n -> do
    let entry = genesisEntry n
        lib = buildLibrary (n - 1)
        (nuPR, _clusters) = proofRankNu entry lib
        pNu = paperNu n

        -- Exact nu at depth 1 (always)
        (_, sc1, _) = computeExactNuAtDepth entry lib 1

        -- Depth 2 only for steps 1-6 (step 7 with 8 atoms is too expensive)
        sc2 = if n <= 6
               then let (_, s, _) = computeExactNuAtDepth entry lib 2 in s
               else -1

        -- Latent bonus breakdown
        pathBonus = length (lePathDims entry)
        maxPathDim = if null (lePathDims entry) then 0
                     else maximum (lePathDims entry)
        homotopyBonus = maxPathDim * maxPathDim
        totalBonus = pathBonus + homotopyBonus
        prSchemaOnly = nuPR - totalBonus
        sanity = if sc1 == prSchemaOnly then "YES" else "NO(" ++ show sc1 ++ "v" ++ show prSchemaOnly ++ ")"
        sc2Str = if sc2 < 0 then "  skip  " else padR 8 (show sc2)

    flushLn $ padR 3 (show n) ++ " | "
            ++ padR 14 (structureName n) ++ " | "
            ++ padR 7 (show pNu) ++ " | "
            ++ padR 4 (show nuPR) ++ " | "
            ++ padR 5 (show totalBonus) ++ " | "
            ++ padR 6 (show prSchemaOnly) ++ " | "
            ++ padR 8 (show sc1) ++ " | "
            ++ sc2Str ++ " | "
            ++ sanity
    ) [1..7]

  flushLn ""
  flushLn "(raw counts: newly inhabited types before schemaization)"
  mapM_ (\n -> do
    let entry = genesisEntry n
        lib = buildLibrary (n - 1)
        (raw1, _, _) = computeExactNuAtDepth entry lib 1
        -- Depth 2 raw only for steps with small libraries
        raw2Str = if n <= 6
                   then let (r2, _, _) = computeExactNuAtDepth entry lib 2 in show r2
                   else "skip"
    flushLn $ "  Step " ++ show n ++ " (" ++ structureName n ++ "): raw_d1=" ++ show raw1 ++ ", raw_d2=" ++ raw2Str
    ) [1..7]

  -- Part 2: Depth 3 skipped (too expensive even for step 1)
  flushLn ""
  flushLn "--- Part 2: Depth 3 (SKIPPED — intractable) ---"
  flushLn ""
  flushLn "  Depth-3 enumeration generates too many types for practical computation."
  flushLn "  Conclusion: depth-2+ exact ν is not useful as a canonical measure."

  -- Part 3: Detailed schema dump for S¹ (step 5) at depth 2
  flushLn ""
  flushLn "--- Part 3: S¹ (step 5) depth-2 schema detail ---"
  let s1Entry = genesisEntry 5
      s1Lib = buildLibrary 4
      (_, _, schemas1) = computeExactNuAtDepth s1Entry s1Lib 1
      (_, _, schemas2) = computeExactNuAtDepth s1Entry s1Lib 2
      schemas1Set = map (prettyTypeExpr . fst) schemas1
      newInD2 = filter (\(s, _) -> prettyTypeExpr s `notElem` schemas1Set) schemas2
  flushLn $ "  Depth-1 schemas (" ++ show (length schemas1) ++ "):"
  mapM_ (\(s, members) ->
    flushLn $ "    " ++ prettyTypeExpr s ++ "  (" ++ show (length members) ++ " members)"
    ) schemas1
  flushLn $ "  New at depth-2 (" ++ show (length newInD2) ++ "):"
  mapM_ (\(s, members) ->
    flushLn $ "    " ++ prettyTypeExpr s ++ "  (" ++ show (length members) ++ " members)"
    ) newInD2

  -- Part 4: S² (step 7) depth-1 schemas only (depth-2 too expensive)
  flushLn ""
  flushLn "--- Part 4: S² (step 7) depth-1 schema detail ---"
  let s2Entry = genesisEntry 7
      s2Lib = buildLibrary 6
      (_, _, schemas7_1) = computeExactNuAtDepth s2Entry s2Lib 1
  flushLn $ "  Depth-1 schemas (" ++ show (length schemas7_1) ++ "):"
  mapM_ (\(s, members) ->
    flushLn $ "    " ++ prettyTypeExpr s ++ "  (" ++ show (length members) ++ " members)"
    ) schemas7_1
  flushLn "  (Depth-2 skipped — 8 atoms makes enumeration intractable)"

  -- Part 5: Key comparison summary
  flushLn ""
  flushLn "--- Part 5: Summary ---"
  flushLn ""
  flushLn "Key question: does depth-2 exact ν explain the latent bonus?"
  flushLn ""
  flushLn " n  | Structure      | ν_paper | PR_schemas | bonus | ν_PR | exact_d2 | d2-d1 | bonus≈d2-d1?"
  flushLn "----|----------------|---------|------------|-------|------|----------|-------|-------------"
  mapM_ (\n -> do
    let entry = genesisEntry n
        lib = buildLibrary (n - 1)
        (nuPR, _) = proofRankNu entry lib
        pNu = paperNu n
        (_, sc1, _) = computeExactNuAtDepth entry lib 1
        sc2 = if n <= 6
               then let (_, s, _) = computeExactNuAtDepth entry lib 2 in s
               else -1
        pathBonus = length (lePathDims entry)
        maxPathDim = if null (lePathDims entry) then 0
                     else maximum (lePathDims entry)
        homotopyBonus = maxPathDim * maxPathDim
        totalBonus = pathBonus + homotopyBonus
        prSchemaOnly = nuPR - totalBonus
    if sc2 < 0
      then flushLn $ padR 3 (show n) ++ " | "
              ++ padR 14 (structureName n) ++ " | "
              ++ padR 7 (show pNu) ++ " | "
              ++ padR 10 (show prSchemaOnly) ++ " | "
              ++ padR 5 (show totalBonus) ++ " | "
              ++ padR 4 (show nuPR) ++ " | "
              ++ "  skip   | skip  | (d2 too expensive)"
      else do
        let d2MinusD1 = sc2 - sc1
            bonusMatch = if totalBonus == 0 && d2MinusD1 == 0 then "---"
                         else if totalBonus == d2MinusD1 then "EXACT"
                         else if abs (totalBonus - d2MinusD1) <= 1 then "~1 off"
                         else show totalBonus ++ " vs " ++ show d2MinusD1
        flushLn $ padR 3 (show n) ++ " | "
                ++ padR 14 (structureName n) ++ " | "
                ++ padR 7 (show pNu) ++ " | "
                ++ padR 10 (show prSchemaOnly) ++ " | "
                ++ padR 5 (show totalBonus) ++ " | "
                ++ padR 4 (show nuPR) ++ " | "
                ++ padR 8 (show sc2) ++ " | "
                ++ padR 5 (show d2MinusD1) ++ " | "
                ++ bonusMatch
    ) [1..7]

  -- Part 6: Multi-horizon kNovelty (compression-drop at H=1..5)
  -- This is the CRITICAL experiment: does compression-drop at low H
  -- match proof-rank? Cost model: atoms=1, unary=1+sub, binary=1+L+R
  -- So H=1 → atoms, H=2 → unary(atom), H=3 → binary(atom,atom)
  -- Depth-1 schemas (X, Omega(X), L->X, L+X, L×X) have costs 1-3.
  -- Hypothesis: kNovelty at H=3 ≈ depth-1 schema count (PR_schemas)
  --             kNovelty at H=4 might capture the homotopy bonus
  flushLn ""
  flushLn "--- Part 6: Multi-Horizon Compression Drop (kNovelty H=1..5) ---"
  flushLn ""
  flushLn "Cost model: atom=1, unary=1+sub, binary=1+L+R"
  flushLn "H=3 covers all depth-1 schemas. H=4-5 adds compositions."
  flushLn ""
  flushLn " n  | Structure      | ν_paper | ν_PR | PR_sch | H=1  | H=2  | H=3  | H=4  | H=5  "
  flushLn "----|----------------|---------|------|--------|------|------|------|------|------"

  mapM_ (\n -> do
    flushLn $ "  Computing step " ++ show n ++ " (" ++ structureName n ++ ")..."
    let entry = genesisEntry n
        lib = buildLibrary (n - 1)
        (nuPR, _) = proofRankNu entry lib
        pNu = paperNu n
        pathBonus_ = length (lePathDims entry)
        maxPD = if null (lePathDims entry) then 0 else maximum (lePathDims entry)
        bonus_ = pathBonus_ + maxPD * maxPD
        prSch = nuPR - bonus_

        -- kNovelty at each horizon
        (k1, _) = kNovelty entry lib 1
        (k2, _) = kNovelty entry lib 2
        (k3, _) = kNovelty entry lib 3
        (k4, _) = kNovelty entry lib 4
        (k5, _) = kNovelty entry lib 5

    flushLn $ padR 3 (show n) ++ " | "
            ++ padR 14 (structureName n) ++ " | "
            ++ padR 7 (show pNu) ++ " | "
            ++ padR 4 (show nuPR) ++ " | "
            ++ padR 6 (show prSch) ++ " | "
            ++ padR 4 (show k1) ++ " | "
            ++ padR 4 (show k2) ++ " | "
            ++ padR 4 (show k3) ++ " | "
            ++ padR 4 (show k4) ++ " | "
            ++ padR 4 (show k5)
    ) [1..8]

  -- Part 7: Detailed kNovelty clusters at H=3 for S¹ (step 5)
  flushLn ""
  flushLn "--- Part 7: S¹ kNovelty cluster detail at H=3 ---"
  let (k3_s1, clusters_s1) = kNovelty (genesisEntry 5) (buildLibrary 4) 3
  flushLn $ "  kNovelty(S¹, H=3) = " ++ show k3_s1 ++ " clusters:"
  mapM_ (\(i, cl) ->
    let names = map prettyTypeExpr (take 3 cl)
        suffix = if length cl > 3 then ", ..." else ""
    in flushLn $ "    [" ++ show i ++ "] size=" ++ show (length cl)
               ++ ": " ++ show names ++ suffix
    ) (zip [1::Int ..] clusters_s1)

  -- Part 8: Detailed kNovelty clusters at H=4 for S¹
  flushLn ""
  flushLn "--- Part 8: S¹ kNovelty cluster detail at H=4 ---"
  let (k4_s1, clusters4_s1) = kNovelty (genesisEntry 5) (buildLibrary 4) 4
  flushLn $ "  kNovelty(S¹, H=4) = " ++ show k4_s1 ++ " clusters:"
  mapM_ (\(i, cl) ->
    let names = map prettyTypeExpr (take 3 cl)
        suffix = if length cl > 3 then ", ..." else ""
    in flushLn $ "    [" ++ show i ++ "] size=" ++ show (length cl)
               ++ ": " ++ show names ++ suffix
    ) (zip [1::Int ..] (take 20 clusters4_s1))
  if length clusters4_s1 > 20
    then flushLn $ "    ... (" ++ show (length clusters4_s1 - 20) ++ " more clusters)"
    else return ()

  flushLn ""
  flushLn "=== Phase M complete ==="

structureName :: Int -> String
structureName 1  = "Universe"
structureName 2  = "Unit"
structureName 3  = "Witness"
structureName 4  = "Pi/Sigma"
structureName 5  = "S1"
structureName 6  = "PropTrunc"
structureName 7  = "S2"
structureName _  = "???"

padR :: Int -> String -> String
padR n s = s ++ replicate (max 0 (n - length s)) ' '
