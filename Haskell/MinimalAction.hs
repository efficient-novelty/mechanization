{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.List (intercalate, nubBy, sortOn)
import Text.Printf (printf)

data Curvature = Riemann | Ricci | ScalarR deriving (Eq, Show)

data Expr
  = EScalarR
  | ELambda
  | EConst String
  | ESquare Expr
  | ETrace Curvature Curvature
  | EPlus Expr Expr
  deriving (Eq, Show)

data Candidate = Candidate
  { name :: String
  , expr :: Expr
  , secondOrderEOM :: Bool
  , divergenceCompatible :: Bool
  , hasCurvatureTerm :: Bool
  }

nodeCount :: Expr -> Int
nodeCount EScalarR = 1
nodeCount (EConst _) = 1
nodeCount ELambda = 1
nodeCount (ESquare a) = 1 + nodeCount a
nodeCount (ETrace _ _) = 1
nodeCount (EPlus a b) = 1 + nodeCount a + nodeCount b

prettyExpr :: Expr -> String
prettyExpr EScalarR = "R"
prettyExpr ELambda = "Λ"
prettyExpr (EConst s) = s
prettyExpr (ESquare x) = "(" <> prettyExpr x <> ")^2"
prettyExpr (ETrace a b) = "Tr(" <> show a <> "·" <> show b <> ")"
prettyExpr (EPlus a b) = prettyExpr a <> " + " <> prettyExpr b

-- Explicit constraint set used by the manuscript text:
-- (1) local scalar invariant
-- (2) second-order equations of motion
-- (3) divergence compatibility
baseCandidates :: [Candidate]
baseCandidates =
  [ Candidate "Einstein-Hilbert" EScalarR True True True
  , Candidate "CosmologicalConstant" ELambda True True False
  , Candidate "R+Lambda" (EPlus EScalarR ELambda) True True True
  , Candidate "R^2" (ESquare EScalarR) False True True
  , Candidate "Ricci^2" (ETrace Ricci Ricci) False True True
  , Candidate "Riemann^2" (ETrace Riemann Riemann) False True True
  ]

admissible :: Candidate -> Bool
admissible c = secondOrderEOM c && divergenceCompatible c && hasCurvatureTerm c

bestDynamicScalar :: [Candidate] -> Maybe Candidate
bestDynamicScalar cs =
  case filter admissible cs of
    [] -> Nothing
    ys -> Just (head (sortOn (nodeCount . expr) ys))

reportCandidate :: Candidate -> String
reportCandidate Candidate {..} =
  printf "%-24s  expr=%-28s  nodes=%d  secondOrder=%s  divCompat=%s  hasCurvature=%s"
    name
    (prettyExpr expr)
    (nodeCount expr)
    (show secondOrderEOM)
    (show divergenceCompatible)
    (show hasCurvatureTerm)

verifyEHMinimal :: [Candidate] -> Either String ()
verifyEHMinimal cs =
  case bestDynamicScalar cs of
    Nothing -> Left "No admissible candidate under constraints"
    Just c
      | name c == "Einstein-Hilbert" -> Right ()
      | otherwise -> Left ("Expected Einstein-Hilbert minimal, got: " <> name c)

main :: IO ()
main = do
  let cs = nubBy (\a b -> name a == name b) baseCandidates
  putStrLn "=== Artifact D: Minimal Action AST Scan ==="
  putStrLn "Constraints: local scalar invariant, curvature term present, second-order EOM, divergence compatibility"
  putStrLn (intercalate "\n" (map reportCandidate cs))
  case verifyEHMinimal cs of
    Right () -> do
      putStrLn "VERDICT: EINSTEIN-HILBERT-MINIMAL"
      putStrLn "Under explicit constraints, Einstein-Hilbert has minimal AST complexity."
    Left err -> do
      putStrLn "VERDICT: FAILED"
      putStrLn err
      error "Artifact D verification failed"
