module Main where

import Kolmogorov (kolmogorovKappaAllSteps, KolmogorovResult(..), genesisSpecs, specBits)

main :: IO ()
main = do
  putStrLn "╔══════════════════════════════════════════════════════════════════════════════╗"
  putStrLn "║  MBTT Kolmogorov κ for all 15 Genesis Steps                                ║"
  putStrLn "╠══════╤════════════════╤═════════╤════════╤════════╤══════════════════════════╣"
  putStrLn "║ Step │ Structure      │ κ_paper │ κ_MBTT │ #Specs │ Best specification      ║"
  putStrLn "╠══════╪════════════════╪═════════╪════════╪════════╪══════════════════════════╣"
  mapM_ printRow kolmogorovKappaAllSteps
  putStrLn "╚══════╧════════════════╧═════════╧════════╧════════╧══════════════════════════╝"
  putStrLn ""
  putStrLn "Detailed specification breakdowns:"
  putStrLn "==================================="
  mapM_ printDetail [1..15]

printRow :: KolmogorovResult -> IO ()
printRow r = putStrLn $ "║ " ++ padR 4 (show (krStep r))
  ++ " │ " ++ padR 14 (krName r)
  ++ " │ " ++ padR 7 (show (krPaperK r))
  ++ " │ " ++ padR 6 (show (krMBTTBits r))
  ++ " │ " ++ padR 6 (show (krSpecCount r))
  ++ " │ " ++ take 24 (krBestSpec r) ++ " ║"

printDetail :: Int -> IO ()
printDetail step = do
  let libSize = step - 1
      specs = genesisSpecs step libSize
  putStrLn $ "\nStep " ++ show step ++ " (lib size = " ++ show libSize ++ "):"
  mapM_ (\(desc, spec) -> 
    putStrLn $ "  " ++ padR 40 desc ++ " → " ++ show (specBits spec) ++ " bits"
    ) specs

padR :: Int -> String -> String
padR n s = take n (s ++ repeat ' ')
