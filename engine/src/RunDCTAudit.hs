-- | DCT Schema Audit
--
-- Dumps ALL schemas for Step 15 (DCT) from the uniform algorithm.
-- Used to verify the ν = 105 count and inspect every schema.

module Main where

import Types (prettyTypeExpr, LibraryEntry(..))
import UniformNu

main :: IO ()
main = do
  putStrLn "DCT Schema Audit (Uniform Algorithm, depth 2)"
  putStrLn "==============================================="
  putStrLn ""

  let results = uniformNuAllSteps 2

  -- Print summary for ALL steps
  putStrLn "Full Uniform ν Table"
  putStrLn "--------------------"
  putStrLn $ padR 5 "Step" ++ padR 15 "Structure" ++ padR 10 "Paper-ν"
          ++ padR 12 "Uniform-ν" ++ padR 10 "Schemas" ++ padR 10 "Raw"
          ++ "Ordering"
  putStrLn $ replicate 80 '-'

  mapM_ (\r -> do
    putStrLn $ padR 5 (show (unrStep r))
            ++ padR 15 (unrName r)
            ++ padR 10 (show (unrPaperNu r))
            ++ padR 12 (show (unrUniformNu r))
            ++ padR 10 (show (unrSchemaCount r))
            ++ padR 10 (show (unrRawNew r))
            ++ unrOrdering r
    ) results

  -- DCT detailed dump
  let dctResult = last results
  putStrLn ""
  putStrLn "============================================"
  putStrLn $ "DCT (Step " ++ show (unrStep dctResult) ++ ")"
  putStrLn "============================================"
  putStrLn $ "Paper ν:   " ++ show (unrPaperNu dctResult)
  putStrLn $ "Uniform ν: " ++ show (unrUniformNu dctResult)
  putStrLn $ "Schema count: " ++ show (unrSchemaCount dctResult)
  putStrLn $ "Raw new types: " ++ show (unrRawNew dctResult)
  putStrLn ""

  -- Per-depth breakdown
  putStrLn "Per-depth breakdown:"
  mapM_ (\(d, raw, sc) ->
    putStrLn $ "  depth " ++ show d ++ ": " ++ show raw ++ " raw, "
            ++ show sc ++ " schemas"
    ) (unrPerDepth dctResult)

  -- Full schema list
  putStrLn ""
  putStrLn $ "All " ++ show (length (unrSchemas dctResult)) ++ " schemas:"
  putStrLn $ replicate 60 '-'
  mapM_ (\(idx, (schema, members)) ->
    putStrLn $ padR 4 (show idx ++ ".")
            ++ padR 40 (prettyTypeExpr schema)
            ++ "(" ++ show (length members) ++ " members)"
    ) (zip [1::Int ..] (unrSchemas dctResult))

  -- Also dump schemas for steps 10-14 (the overcounted ones)
  putStrLn ""
  putStrLn "============================================"
  putStrLn "Steps 10-14 Schema Counts (overcounting audit)"
  putStrLn "============================================"
  mapM_ (\r -> do
    let step = unrStep r
    if step >= 10 && step <= 14
      then do
        putStrLn $ "\nStep " ++ show step ++ " (" ++ unrName r ++ "):"
                ++ " paper=" ++ show (unrPaperNu r)
                ++ " uniform=" ++ show (unrUniformNu r)
                ++ " schemas=" ++ show (unrSchemaCount r)
        mapM_ (\(schema, members) ->
          putStrLn $ "    " ++ prettyTypeExpr schema
                  ++ " (" ++ show (length members) ++ " members)"
          ) (unrSchemas r)
      else return ()
    ) results

padR :: Int -> String -> String
padR n s = s ++ replicate (max 0 (n - length s)) ' '
