-- | Standalone executable for inference-rule-based ν computation
--
-- Runs the inference-rule counter on all 15 Genesis structures
-- and outputs a comparison table with paper values.

module Main where

import InferenceNu

main :: IO ()
main = do
  putStrLn "PEN Inference-Rule-Based ν (Generative Capacity)"
  putStrLn "================================================="
  putStrLn ""
  putStrLn "Counts atomic inference rules (Intro/Elim/Comp) per Genesis step."
  putStrLn ""

  let results = inferenceNuAllSteps

  -- Print comparison table
  putStrLn $ padR 5 "Step" ++ padR 15 "Structure"
          ++ padR 10 "Paper-ν" ++ padR 8 "ν_G" ++ padR 8 "ν_C"
          ++ padR 8 "ν_H" ++ padR 10 "ν_total" ++ padR 8 "Delta"
          ++ "Ordering"
  putStrLn $ replicate 95 '-'

  mapM_ (\r -> do
    let delta = inrTotal r - inrPaperNu r
        deltaStr = if delta == 0 then "  0"
                   else if delta > 0 then " +" ++ show delta
                   else " " ++ show delta
    putStrLn $ padR 5 (show (inrStep r))
            ++ padR 15 (inrName r)
            ++ padR 10 (show (inrPaperNu r))
            ++ padR 8 (show (inrNuG r))
            ++ padR 8 (show (inrNuC r))
            ++ padR 8 (show (inrNuH r))
            ++ padR 10 (show (inrTotal r))
            ++ padR 8 deltaStr
            ++ inrOrdering r
    ) results

  -- Summary
  putStrLn ""
  let exact = length [r | r <- results, inrTotal r == inrPaperNu r]
      total = length results
  putStrLn $ "Exact match: " ++ show exact ++ " / " ++ show total

  if exact == total
    then putStrLn "ALL 15 STRUCTURES VERIFIED — 15/15 MATCH"
    else do
      putStrLn $ "WARNING: " ++ show (total - exact) ++ " mismatches remain:"
      mapM_ (\r ->
        putStrLn $ "  Step " ++ show (inrStep r) ++ " " ++ inrName r
                ++ ": paper=" ++ show (inrPaperNu r)
                ++ " computed=" ++ show (inrTotal r)
                ++ " delta=" ++ show (inrTotal r - inrPaperNu r)
        ) [r | r <- results, inrTotal r /= inrPaperNu r]

  -- Spectral decomposition summary
  putStrLn ""
  putStrLn "Spectral Decomposition Summary"
  putStrLn "-------------------------------"
  let totalNuG = sum [inrNuG r | r <- results]
      totalNuC = sum [inrNuC r | r <- results]
      totalNuH = sum [inrNuH r | r <- results]
      grandTotal = totalNuG + totalNuC + totalNuH
  putStrLn $ "  Total ν_G (Grammar/Intro):     " ++ show totalNuG
          ++ " (" ++ showPct totalNuG grandTotal ++ ")"
  putStrLn $ "  Total ν_C (Capability/Elim):   " ++ show totalNuC
          ++ " (" ++ showPct totalNuC grandTotal ++ ")"
  putStrLn $ "  Total ν_H (Homotopy/Comp):     " ++ show totalNuH
          ++ " (" ++ showPct totalNuH grandTotal ++ ")"
  putStrLn $ "  Grand total:                   " ++ show grandTotal

showPct :: Int -> Int -> String
showPct n d =
  let pct = fromIntegral n * 100.0 / fromIntegral d :: Double
      rounded = fromIntegral (round (pct * 10) :: Int) / 10.0 :: Double
  in show rounded ++ "%"

padR :: Int -> String -> String
padR n s = s ++ replicate (max 0 (n - length s)) ' '
