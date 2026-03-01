
import MBTTNu
import Telescope
import TelescopeEval
import Types
import Kolmogorov (MBTTExpr(..))
import Data.List (isPrefixOf)

baseSteps :: [Int]
baseSteps = [1..12]

buildBase :: (Library, [(Int,Int)])
buildBase = go [] [] baseSteps
  where
    go lib nuHist [] = (lib, nuHist)
    go lib nuHist (s:ss) =
      let tele = referenceTelescope s
          nm = detectCanonicalName tele lib
          nr = computeNativeNu tele lib nuHist
          entry = telescopeToCandidate tele lib nm
      in go (lib ++ [entry]) (nuHist ++ [(s, nnTotal nr)]) ss

scalarTele :: String -> Telescope
scalarTele route = case route of
  "cauchy_minimal" -> Telescope
    [ TeleEntry "N-form" (App Univ (Var 1))
    , TeleEntry "Z-form" (Sigma (Var 1) (Var 1))
    , TeleEntry "Q-form" (Sigma (Var 1) (Pi (Var 1) (Var 1)))
    , TeleEntry "R-form" (Pi (Var 1) (Var 1))
    ]
  "topological_arithmetic" -> Telescope
    [ TeleEntry "Z-loop" (App (Lib 5) (Var 1))
    , TeleEntry "Q-quot" (Sigma (Var 1) (Var 1))
    , TeleEntry "R-comp" (Pi (Var 1) (Var 1))
    ]
  "synthetic_continuum" -> Telescope
    [ TeleEntry "R-smooth" (App (Lib 10) (Var 1))
    , TeleEntry "R-ring" (Sigma (Pi (Var 1) (Var 1)) (Pi (Var 1) (Var 1)))
    ]
  _ -> Telescope []

kappaScalar :: String -> Int
kappaScalar route = case route of
  "cauchy_minimal" -> 4
  "topological_arithmetic" -> 3
  "synthetic_continuum" -> 2
  _ -> 0

nodeLines :: NativeNuResult -> [String]
nodeLines nr = filter (isPrefixOf "node=") (nnTrace nr)

emitRoute :: String -> IO ()
emitRoute route = do
  let (libBase, nuHist) = buildBase
      tScalar = scalarTele route
      tMetric = referenceTelescope 13
      tCombined = Telescope (teleEntries tScalar ++ teleEntries tMetric)
      nScalar = computeNativeNu tScalar libBase nuHist
      nMetric = computeNativeNu tMetric libBase nuHist
      nCombined = computeNativeNu tCombined libBase nuHist
      kS = kappaScalar route
      kF = 7 + kS
      dMin = ceiling (5.99 * fromIntegral kF - 46.0) :: Int
      dMech = nnTotal nCombined - nnTotal nMetric - nnTotal nScalar
  putStrLn $ "ROUTE|" ++ route ++ "|" ++ show kS ++ "|" ++ show kF ++ "|"
          ++ show (nnTotal nScalar) ++ "|" ++ show (nnTotal nMetric) ++ "|"
          ++ show (nnTotal nCombined) ++ "|" ++ show dMin ++ "|" ++ show dMech
  mapM_ (\ln -> putStrLn $ "TRACE|" ++ route ++ "|scalar|" ++ ln) (nodeLines nScalar)
  mapM_ (\ln -> putStrLn $ "TRACE|" ++ route ++ "|metric|" ++ ln) (nodeLines nMetric)
  mapM_ (\ln -> putStrLn $ "TRACE|" ++ route ++ "|combined|" ++ ln) (nodeLines nCombined)

main :: IO ()
main = do
  mapM_ emitRoute ["cauchy_minimal","topological_arithmetic","synthetic_continuum"]
