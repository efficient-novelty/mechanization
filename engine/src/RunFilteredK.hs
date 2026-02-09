-- | Filtered kNovelty experiment
-- Decomposes kNovelty(H=3) clusters into: trivial, susp-chain, core
-- Investigates X→L (reversed direction), constant-specific schemas,
-- and whether |susp-chains| ≈ homotopy bonus.
-- Run with: cabal run filtered-k

module Main where

import Types
import KappaNu (genesisEntry, buildLibrary, paperNu)
import Cluster (proofRankNu)
import ProofRank (kNovelty, schemaize, normalize)
import System.IO (hFlush, stdout)

flushLn :: String -> IO ()
flushLn s = putStrLn s >> hFlush stdout

-- ============================================
-- Schema Classification
-- ============================================

data SchemaClass
  = Trivial        -- X→X, X×X, X+X, SelfId(X)
  | SuspChain      -- Susp(...)
  | ReversedArrow  -- X→L (free mapping space)
  | ConstSpecific  -- involves TUnit or TVoid (e.g. 1+X)
  | CoreSchema     -- matches proof-rank schemas (L+X, L→X, L×X, X, Omega(X), etc.)
  deriving (Show, Eq)

classifySchema :: TypeExpr -> SchemaClass
classifySchema s
  | isTrivial s      = Trivial
  | isSusp s         = SuspChain
  | isRevArrow s     = ReversedArrow
  | hasConstant s    = ConstSpecific
  | otherwise        = CoreSchema

-- | Trivial schemas (same as Independence.isTrivialSchema)
isTrivial :: TypeExpr -> Bool
isTrivial (TProd (TRef "X") (TRef "X")) = True
isTrivial (TCoprod (TRef "X") (TRef "X")) = True
isTrivial (TArrow (TRef "X") (TRef "X")) = True
isTrivial (TSelfId (TRef "X")) = True
isTrivial _ = False

-- | Susp-chain: outermost constructor is TSusp
isSusp :: TypeExpr -> Bool
isSusp (TSusp _) = True
isSusp _ = False

-- | Reversed arrow: X → L
isRevArrow :: TypeExpr -> Bool
isRevArrow (TArrow (TRef "X") (TRef "L")) = True
isRevArrow _ = False

-- | Contains TUnit or TVoid (constant-specific, not pure L/X)
hasConstant :: TypeExpr -> Bool
hasConstant TUnit = True
hasConstant TVoid = True
hasConstant (TRef _) = False
hasConstant (TArrow a b) = hasConstant a || hasConstant b
hasConstant (TProd a b) = hasConstant a || hasConstant b
hasConstant (TCoprod a b) = hasConstant a || hasConstant b
hasConstant (TSelfId a) = hasConstant a
hasConstant (TOmega a) = hasConstant a
hasConstant (TSusp a) = hasConstant a
hasConstant (TTrunc _ a) = hasConstant a
hasConstant (TId a x y) = hasConstant a || hasConstant x || hasConstant y
hasConstant (TPi _ a b) = hasConstant a || hasConstant b
hasConstant (TSigma _ a b) = hasConstant a || hasConstant b
hasConstant (THIT _ _) = False
hasConstant (TFiber a b) = hasConstant a || hasConstant b
hasConstant (TDeloop a) = hasConstant a

-- ============================================
-- Main
-- ============================================

main :: IO ()
main = do
  flushLn "Filtered kNovelty Experiment"
  flushLn "============================"
  flushLn ""
  flushLn "Decomposes kNovelty(H=3) clusters by schema class:"
  flushLn "  Trivial:  X->X, X*X, X+X, SelfId(X)"
  flushLn "  Susp:     Susp(...)"
  flushLn "  RevArrow: X->L (free mapping space)"
  flushLn "  Const:    involves 1 or 0 (e.g. 1+X)"
  flushLn "  Core:     pure L/X schemas (should match proof-rank)"
  flushLn ""

  -- Part 1: Summary table
  flushLn "--- Part 1: Decomposition summary ---"
  flushLn ""
  flushLn " n  | Structure  | kN_H3 | triv | susp | rev  | const | core | PR_sch | core==PR?"
  flushLn "----|------------|-------|------|------|------|-------|------|--------|----------"

  mapM_ (\n -> do
    let entry = genesisEntry n
        lib = buildLibrary (n - 1)
        name = leName entry
        (nuPR, _) = proofRankNu entry lib
        pathBonus_ = length (lePathDims entry)
        maxPD = if null (lePathDims entry) then 0 else maximum (lePathDims entry)
        bonus_ = pathBonus_ + maxPD * maxPD
        prSch = nuPR - bonus_

        (kn3, clusters3) = kNovelty entry lib 3
        classified = classifyClusters name lib clusters3

        trivCount  = countClass Trivial classified
        suspCount  = countClass SuspChain classified
        revCount   = countClass ReversedArrow classified
        constCount = countClass ConstSpecific classified
        coreCount  = countClass CoreSchema classified

        coreMatch = if coreCount == prSch then "YES"
                    else "NO(" ++ show coreCount ++ "v" ++ show prSch ++ ")"

    flushLn $ padR 3 (show n) ++ " | "
            ++ padR 10 (structureName n) ++ " | "
            ++ padR 5 (show kn3) ++ " | "
            ++ padR 4 (show trivCount) ++ " | "
            ++ padR 4 (show suspCount) ++ " | "
            ++ padR 4 (show revCount) ++ " | "
            ++ padR 5 (show constCount) ++ " | "
            ++ padR 4 (show coreCount) ++ " | "
            ++ padR 6 (show prSch) ++ " | "
            ++ coreMatch
    ) [1..8]

  -- Part 2: Detailed schema classification for each step
  flushLn ""
  flushLn "--- Part 2: Detailed schema classification at H=3 ---"

  mapM_ (\n -> do
    let entry = genesisEntry n
        lib = buildLibrary (n - 1)
        name = leName entry
        (_, clusters3) = kNovelty entry lib 3
        classified = classifyClusters name lib clusters3

    flushLn ""
    flushLn $ "  Step " ++ show n ++ " (" ++ structureName n ++ "):"
    mapM_ (\(cls, schema, cl) ->
      flushLn $ "    " ++ padR 12 (show cls) ++ "  "
             ++ padR 30 (prettyTypeExpr schema)
             ++ " (" ++ show (length cl) ++ " members)"
      ) classified
    ) [1..8]

  -- Part 3: Susp-chain count vs homotopy bonus
  flushLn ""
  flushLn "--- Part 3: Susp-chain count vs homotopy bonus ---"
  flushLn ""
  flushLn " n  | Structure  | pathB | maxPD^2 | bonus | #susp | susp==bonus?"
  flushLn "----|------------|-------|---------|-------|-------|-------------"

  mapM_ (\n -> do
    let entry = genesisEntry n
        lib = buildLibrary (n - 1)
        name = leName entry
        (_, clusters3) = kNovelty entry lib 3
        classified = classifyClusters name lib clusters3
        suspCount = countClass SuspChain classified

        pathBonus_ = length (lePathDims entry)
        maxPD = if null (lePathDims entry) then 0 else maximum (lePathDims entry)
        homotopyBonus = maxPD * maxPD
        totalBonus = pathBonus_ + homotopyBonus

        match = if suspCount == 0 && totalBonus == 0 then "---"
                else if suspCount == totalBonus then "EXACT"
                else if abs (suspCount - totalBonus) <= 1 then "~1 off"
                else show suspCount ++ " vs " ++ show totalBonus

    flushLn $ padR 3 (show n) ++ " | "
            ++ padR 10 (structureName n) ++ " | "
            ++ padR 5 (show pathBonus_) ++ " | "
            ++ padR 7 (show homotopyBonus) ++ " | "
            ++ padR 5 (show totalBonus) ++ " | "
            ++ padR 5 (show suspCount) ++ " | "
            ++ match
    ) [1..8]

  -- Part 4: X→L investigation — if we include X→L as core, does it fix everything?
  flushLn ""
  flushLn "--- Part 4: X->L inclusion test ---"
  flushLn ""
  flushLn "If X->L (free mapping space) is a genuine core schema:"
  flushLn "  core' = core + rev"
  flushLn "  Then nu_grammar = core', nu_homotopy = susp_chains"
  flushLn "  And nu = core' + susp should match nu_paper"
  flushLn ""
  flushLn " n  | Structure  | ν_paper | core | rev | core' | susp | core'+susp | match?"
  flushLn "----|------------|---------|------|-----|-------|------|------------|-------"

  mapM_ (\n -> do
    let entry = genesisEntry n
        lib = buildLibrary (n - 1)
        name = leName entry
        pNu = paperNu n
        (_, clusters3) = kNovelty entry lib 3
        classified = classifyClusters name lib clusters3

        coreCount = countClass CoreSchema classified
        revCount  = countClass ReversedArrow classified
        suspCount = countClass SuspChain classified

        corePrime = coreCount + revCount
        total = corePrime + suspCount
        match = if total == pNu then "YES"
                else "NO(" ++ show total ++ " vs " ++ show pNu ++ ")"

    flushLn $ padR 3 (show n) ++ " | "
            ++ padR 10 (structureName n) ++ " | "
            ++ padR 7 (show pNu) ++ " | "
            ++ padR 4 (show coreCount) ++ " | "
            ++ padR 3 (show revCount) ++ " | "
            ++ padR 5 (show corePrime) ++ " | "
            ++ padR 4 (show suspCount) ++ " | "
            ++ padR 10 (show total) ++ " | "
            ++ match
    ) [1..8]

  -- Part 5: Alternative bonus formulas
  flushLn ""
  flushLn "--- Part 5: What bonus formula makes core+bonus = ν_paper? ---"
  flushLn ""
  flushLn "needed_bonus = ν_paper - core (without X->L)"
  flushLn "needed_bonus' = ν_paper - core' (with X->L)"
  flushLn ""
  flushLn " n  | Structure  | ν_paper | core | core' | needed | needed' | susp | current_bonus"
  flushLn "----|------------|---------|------|-------|--------|---------|------|-------------"

  mapM_ (\n -> do
    let entry = genesisEntry n
        lib = buildLibrary (n - 1)
        name = leName entry
        pNu = paperNu n
        (nuPR, _) = proofRankNu entry lib
        pathBonus_ = length (lePathDims entry)
        maxPD = if null (lePathDims entry) then 0 else maximum (lePathDims entry)
        curBonus = pathBonus_ + maxPD * maxPD

        (_, clusters3) = kNovelty entry lib 3
        classified = classifyClusters name lib clusters3

        coreCount = countClass CoreSchema classified
        revCount  = countClass ReversedArrow classified
        suspCount = countClass SuspChain classified

        corePrime = coreCount + revCount
        needed = pNu - coreCount
        neededPrime = pNu - corePrime

    flushLn $ padR 3 (show n) ++ " | "
            ++ padR 10 (structureName n) ++ " | "
            ++ padR 7 (show pNu) ++ " | "
            ++ padR 4 (show coreCount) ++ " | "
            ++ padR 5 (show corePrime) ++ " | "
            ++ padR 6 (show needed) ++ " | "
            ++ padR 7 (show neededPrime) ++ " | "
            ++ padR 4 (show suspCount) ++ " | "
            ++ show curBonus
    ) [1..8]

  -- Part 6: Susp-chain detail for S¹ and S²
  flushLn ""
  flushLn "--- Part 6: Susp-chain schemas for S¹ and S² ---"
  flushLn ""

  let printSuspDetail stepN = do
        let entry = genesisEntry stepN
            lib = buildLibrary (stepN - 1)
            name = leName entry
            (_, clusters3) = kNovelty entry lib 3
            classified = classifyClusters name lib clusters3
            suspSchemas = [(schema, cl) | (SuspChain, schema, cl) <- classified]
        flushLn $ "  " ++ structureName stepN ++ " (step " ++ show stepN ++ ") Susp chains:"
        mapM_ (\(schema, cl) ->
          flushLn $ "    " ++ prettyTypeExpr schema
                 ++ "  (" ++ show (length cl) ++ " members)"
          ) suspSchemas

  printSuspDetail 5  -- S¹
  flushLn ""
  printSuspDetail 7  -- S²
  flushLn ""
  printSuspDetail 8  -- S³

  flushLn ""
  flushLn "=== Done ==="

-- ============================================
-- Helpers
-- ============================================

classifyClusters :: String -> Library -> [[TypeExpr]]
                 -> [(SchemaClass, TypeExpr, [TypeExpr])]
classifyClusters name lib clusters =
  map (\cl ->
    let rep = head cl
        schema = normalize (schemaize name lib rep)
        cls = classifySchema schema
    in (cls, schema, cl)
  ) clusters

countClass :: SchemaClass -> [(SchemaClass, TypeExpr, [TypeExpr])] -> Int
countClass cls classified = length [() | (c, _, _) <- classified, c == cls]

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
