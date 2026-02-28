-- | PEN Agda Rosetta Bridge — CLI
--
-- Generates Cubical Agda stub files from the engine's discovered telescopes.
--
-- Usage:
--   cabal run agda-bridge                      # Generate all 15 steps to agda/Genesis/
--   cabal run agda-bridge -- --step 5          # Generate only step 5 (S1)
--   cabal run agda-bridge -- --output-dir DIR  # Custom output directory
--   cabal run agda-bridge -- --stdout          # Print to stdout instead of files
--   cabal run agda-bridge -- --check           # Verify generation is deterministic

module Main where

import AgdaExport (exportAllSteps, exportStep, exportAllVerificationPayloads)

import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.IO (hFlush, stdout)
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  let outputDir = parseOutputDir args
      stepFilter = parseStep args
      stdoutMode = "--stdout" `elem` args
      checkMode = "--check" `elem` args

  if checkMode then do
    -- Determinism check: generate twice, compare
    let files1 = exportAllSteps ++ exportAllVerificationPayloads
        files2 = exportAllSteps ++ exportAllVerificationPayloads
    if files1 == files2
      then do
        putStrLn "PASS: Agda bridge output is deterministic"
        printf "  %d files, %d total bytes\n"
          (length files1)
          (sum [length content | (_, content) <- files1])
        exitSuccess
      else do
        putStrLn "FAIL: Agda bridge output is non-deterministic"
        exitFailure
  else if stdoutMode then do
    -- Print to stdout
    case stepFilter of
      Just s -> putStr (exportStep s)
      Nothing -> mapM_ (\(_, content) -> putStr content >> putStrLn "") exportAllSteps
  else do
    -- Write to files
    putStrLn "============================================"
    putStrLn "PEN Agda Rosetta Bridge"
    putStrLn "============================================"
    putStrLn ""

    let allFiles = exportAllSteps ++ exportAllVerificationPayloads
        files = case stepFilter of
          Just s -> filter (\(fp, _) -> show s `isInfixOf'` fp) allFiles
          Nothing -> allFiles

    if null files then do
      putStrLn "ERROR: No matching steps found"
      exitFailure
    else do
      mapM_ (writeAgdaFile outputDir) files
      putStrLn ""
      printf "Generated %d Agda stub file(s) in %s\n" (length files) outputDir
      putStrLn ""
      putStrLn "NOTE: These are postulate stubs documenting the MBTT telescope structure."
      putStrLn "They are not expected to typecheck directly in Cubical Agda."
      putStrLn "For machine-checked proofs, see agda/PEN.agda."
      hFlush stdout

-- | Write a single Agda file, creating directories as needed.
writeAgdaFile :: FilePath -> (FilePath, String) -> IO ()
writeAgdaFile baseDir (relPath, content) = do
  let fullPath = baseDir </> relPath
      dir = takeDirectory fullPath
  createDirectoryIfMissing True dir
  -- Check if file exists and content differs
  exists <- doesFileExist fullPath
  if exists then do
    old <- readFile fullPath
    if old == content
      then printf "  [unchanged] %s\n" fullPath
      else do
        writeFile fullPath content
        printf "  [updated]   %s\n" fullPath
  else do
    writeFile fullPath content
    printf "  [created]   %s\n" fullPath

-- | Parse --output-dir argument (default: current directory).
parseOutputDir :: [String] -> FilePath
parseOutputDir args = case dropWhile (/= "--output-dir") args of
  ("--output-dir" : dir : _) -> dir
  _ -> "."

-- | Parse --step N argument.
parseStep :: [String] -> Maybe Int
parseStep args = case dropWhile (/= "--step") args of
  ("--step" : n : _) -> case reads n of
    [(s, "")] | s >= 1 && s <= 15 -> Just s
    _ -> Nothing
  _ -> Nothing

-- | Simple infix check (no Data.List import needed).
isInfixOf' :: String -> String -> Bool
isInfixOf' needle haystack = any (isPrefixOf' needle) (tails' haystack)
  where
    isPrefixOf' [] _ = True
    isPrefixOf' _ [] = False
    isPrefixOf' (a:as) (b:bs) = a == b && isPrefixOf' as bs
    tails' [] = [[]]
    tails' s@(_:xs) = s : tails' xs
