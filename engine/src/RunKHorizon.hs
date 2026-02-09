-- | Focused runner: Multi-horizon kNovelty comparison
-- Tests compression-drop measure at H=1..5 against proof-rank
-- Run with: cabal run k-horizon

module Main where

import Types
import KappaNu (genesisEntry, buildLibrary, paperNu)
import Cluster (proofRankNu)
import ProofRank (kNovelty)
import System.IO (hFlush, stdout)

flushLn :: String -> IO ()
flushLn s = putStrLn s >> hFlush stdout

main :: IO ()
main = do
  flushLn "Multi-Horizon Compression Drop (kNovelty H=1..5)"
  flushLn "================================================="
  flushLn ""
  flushLn "Cost model: atom=1, unary=1+sub, binary=1+L+R"
  flushLn "H=3 covers all depth-1 schemas. H=4-5 adds compositions."
  flushLn ""
  flushLn " n  | Structure      | ν_paper | ν_PR | bonus | PR_sch | H=1  | H=2  | H=3  | H=4  | H=5  "
  flushLn "----|----------------|---------|------|-------|--------|------|------|------|------|------"

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
            ++ padR 5 (show bonus_) ++ " | "
            ++ padR 6 (show prSch) ++ " | "
            ++ padR 4 (show k1) ++ " | "
            ++ padR 4 (show k2) ++ " | "
            ++ padR 4 (show k3) ++ " | "
            ++ padR 4 (show k4) ++ " | "
            ++ padR 4 (show k5)
    ) [1..8]

  -- Detailed kNovelty clusters at H=3 for S¹ (step 5)
  flushLn ""
  flushLn "--- S¹ kNovelty cluster detail at H=3 ---"
  let (k3_s1, clusters_s1) = kNovelty (genesisEntry 5) (buildLibrary 4) 3
  flushLn $ "  kNovelty(S¹, H=3) = " ++ show k3_s1 ++ " clusters:"
  mapM_ (\(i, cl) ->
    let names = map prettyTypeExpr (take 4 cl)
        suffix = if length cl > 4 then ", ..." else ""
    in flushLn $ "    [" ++ show i ++ "] size=" ++ show (length cl)
               ++ ": {" ++ unwords (map (++ ",") (init names)) ++ " " ++ last names ++ suffix ++ "}"
    ) (zip [1::Int ..] clusters_s1)

  -- Detailed kNovelty clusters at H=4 for S¹
  flushLn ""
  flushLn "--- S¹ kNovelty cluster detail at H=4 (first 25) ---"
  let (k4_s1, clusters4_s1) = kNovelty (genesisEntry 5) (buildLibrary 4) 4
  flushLn $ "  kNovelty(S¹, H=4) = " ++ show k4_s1 ++ " clusters:"
  mapM_ (\(i, cl) ->
    let names = map prettyTypeExpr (take 3 cl)
        suffix = if length cl > 3 then ", ..." else ""
    in flushLn $ "    [" ++ show i ++ "] size=" ++ show (length cl)
               ++ ": {" ++ concatMap (++ ", ") (init names) ++ last names ++ suffix ++ "}"
    ) (zip [1::Int ..] (take 25 clusters4_s1))
  if length clusters4_s1 > 25
    then flushLn $ "    ... (" ++ show (length clusters4_s1 - 25) ++ " more clusters)"
    else return ()

  -- S² at H=3 and H=4
  flushLn ""
  flushLn "--- S² kNovelty cluster detail at H=3 ---"
  let (k3_s2, clusters_s2_3) = kNovelty (genesisEntry 7) (buildLibrary 6) 3
  flushLn $ "  kNovelty(S², H=3) = " ++ show k3_s2 ++ " clusters:"
  mapM_ (\(i, cl) ->
    let names = map prettyTypeExpr (take 4 cl)
        suffix = if length cl > 4 then ", ..." else ""
    in flushLn $ "    [" ++ show i ++ "] size=" ++ show (length cl)
               ++ ": {" ++ concatMap (++ ", ") (init names) ++ last names ++ suffix ++ "}"
    ) (zip [1::Int ..] clusters_s2_3)

  flushLn ""
  flushLn "=== Done ==="

structureName :: Int -> String
structureName 1  = "Universe"
structureName 2  = "Unit"
structureName 3  = "Witness"
structureName 4  = "Pi/Sigma"
structureName 5  = "S1"
structureName 6  = "PropTrunc"
structureName 7  = "S2"
structureName 8  = "S3"
structureName _  = "???"

padR :: Int -> String -> String
padR n s = s ++ replicate (max 0 (n - length s)) ' '
