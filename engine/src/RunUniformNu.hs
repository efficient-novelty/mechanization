{-# LANGUAGE BangPatterns #-}

-- | Standalone executable for Priority 2: Uniform Nu Computation
--
-- Runs the uniform nu algorithm on all 15 Genesis structures
-- and outputs a comparison table with paper values.
--
-- Usage:
--   cabal run uniform-nu              -- depth 1 (fast, baseline)
--   cabal run uniform-nu -- --depth 2 -- depth 2 (full, slower)

module Main where

import Types (LibraryEntry(..), prettyTypeExpr)
import UniformNu
import System.Environment (getArgs)
import Data.List (intercalate)

main :: IO ()
main = do
  args <- getArgs
  let maxDepth = parseDepth args

  putStrLn $ "PEN Priority 2: Uniform Nu Computation"
  putStrLn $ "======================================="
  putStrLn $ "Max expression depth: " ++ show maxDepth
  putStrLn ""

  -- Run the uniform algorithm on all 15 steps
  putStrLn "Computing uniform nu for all 15 Genesis structures..."
  putStrLn "(Using ALL library atoms, no domain knowledge)"
  putStrLn ""

  let results = uniformNuAllSteps maxDepth

  -- Print comparison table
  printComparisonTable results

  -- Print per-depth breakdown for interesting steps
  putStrLn ""
  putStrLn "Per-Depth Breakdown"
  putStrLn "-------------------"
  mapM_ printDepthBreakdown results

  -- Print schemas for steps where uniform differs from paper
  putStrLn ""
  putStrLn "Schema Details (steps with delta > 0)"
  putStrLn "--------------------------------------"
  mapM_ printSchemaDetails (filter (\r -> unrUniformNu r /= unrPaperNu r) results)

  -- Summary statistics
  putStrLn ""
  printSummary results

parseDepth :: [String] -> Int
parseDepth ("--depth":d:_) = read d
parseDepth _ = 1  -- default to depth 1 for speed

printComparisonTable :: [UniformNuResult] -> IO ()
printComparisonTable results = do
  putStrLn $ padR 4 "Step" ++ padR 14 "Structure"
          ++ padR 10 "Paper-nu" ++ padR 12 "Uniform-nu"
          ++ padR 8 "Delta" ++ "Ordering"
  putStrLn $ replicate 70 '-'
  mapM_ printRow results
  where
    printRow r =
      let delta = unrUniformNu r - unrPaperNu r
          deltaStr = if delta == 0 then "  0"
                     else if delta > 0 then " +" ++ show delta
                     else " " ++ show delta
      in putStrLn $ padR 4 (show (unrStep r))
                 ++ padR 14 (unrName r)
                 ++ padR 10 (show (unrPaperNu r))
                 ++ padR 12 (show (unrUniformNu r))
                 ++ padR 8 deltaStr
                 ++ unrOrdering r

printDepthBreakdown :: UniformNuResult -> IO ()
printDepthBreakdown r = do
  putStrLn $ "  Step " ++ show (unrStep r) ++ " (" ++ unrName r ++ "):"
  mapM_ (\(d, raw, sc) ->
    putStrLn $ "    depth " ++ show d ++ ": " ++ show raw ++ " raw, "
            ++ show sc ++ " schemas") (unrPerDepth r)

printSchemaDetails :: UniformNuResult -> IO ()
printSchemaDetails r = do
  putStrLn $ "\n  Step " ++ show (unrStep r) ++ " (" ++ unrName r
          ++ "): paper=" ++ show (unrPaperNu r)
          ++ " uniform=" ++ show (unrUniformNu r)
  mapM_ (\(schema, members) ->
    putStrLn $ "    " ++ prettyTypeExpr schema
            ++ " (" ++ show (length members) ++ " members)")
    (take 20 (unrSchemas r))
  let total = length (unrSchemas r)
  if total > 20
    then putStrLn $ "    ... (" ++ show (total - 20) ++ " more schemas)"
    else return ()

printSummary :: [UniformNuResult] -> IO ()
printSummary results = do
  let exact = length [r | r <- results, unrUniformNu r == unrPaperNu r]
      close = length [r | r <- results, abs (unrUniformNu r - unrPaperNu r) <= 2]
      orderOk = length [r | r <- results, take 2 (unrOrdering r) == "OK"]
  putStrLn "Summary"
  putStrLn "-------"
  putStrLn $ "  Exact match: " ++ show exact ++ " / 15"
  putStrLn $ "  Within +/-2: " ++ show close ++ " / 15"
  putStrLn $ "  Ordering preserved: " ++ show orderOk ++ " / 15"

padR :: Int -> String -> String
padR n s = s ++ replicate (max 0 (n - length s)) ' '
