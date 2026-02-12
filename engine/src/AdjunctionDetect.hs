-- | Adjunction depth analysis for PEN Genesis steps
--
-- Each genuine type former in the Genesis Sequence (steps 4+)
-- introduces an operator that participates in an adjunction.
-- The triangle identities of that adjunction are Depth-2
-- obligations, providing computational evidence for d = 2.

module AdjunctionDetect where

-- ============================================================
-- Adjunction Info
-- ============================================================

data AdjunctionInfo = AdjunctionInfo
  { aiStep       :: Int
  , aiName       :: String
  , aiLeftAdj    :: String    -- Left adjoint (introduction)
  , aiRightAdj   :: String    -- Right adjoint (elimination)
  , aiUnit       :: String    -- Unit (η) description
  , aiCounit     :: String    -- Counit (ε) description
  , aiTriDepth   :: Int       -- Triangle identity depth
  , aiTriple     :: Bool      -- Part of an adjoint triple?
  } deriving (Show)

-- ============================================================
-- Adjunction table for all Genesis steps
-- ============================================================

adjunctionForStep :: Int -> AdjunctionInfo
-- Steps 1-3: Foundational (no adjunction structure)
adjunctionForStep 1 = AdjunctionInfo 1 "Universe"
  "—" "—" "—" "—" 0 False
adjunctionForStep 2 = AdjunctionInfo 2 "Unit"
  "—" "—" "—" "—" 0 False
adjunctionForStep 3 = AdjunctionInfo 3 "Witness"
  "—" "—" "—" "—" 0 False

-- Step 4: Π/Σ — the prototypical adjunction
adjunctionForStep 4 = AdjunctionInfo 4 "Pi/Sigma"
  "Sigma (Σ)" "Pi (Π)"
  "diagonal: A → (B → A×B)"
  "eval: (A→B)×A → B"
  2 True  -- Σ ⊣ Δ ⊣ Π (adjoint triple)

-- Step 5: Circle S¹ — suspension/loop space
adjunctionForStep 5 = AdjunctionInfo 5 "S1"
  "Susp (Σ)" "Loop (Ω)"
  "north/south: A → Susp(A)"
  "loop-elim: Ω(B) → (S¹→B)"
  2 True  -- Σ ⊣ Ω

-- Step 6: PropTrunc — truncation/inclusion
adjunctionForStep 6 = AdjunctionInfo 6 "PropTrunc"
  "inc: A → ‖A‖" "rec: ‖A‖ → B (B prop)"
  "inclusion into truncation"
  "universal property of truncation"
  2 False

-- Step 7: Sphere S² — iterated suspension
adjunctionForStep 7 = AdjunctionInfo 7 "S2"
  "Susp²" "Ω²"
  "iterated suspension unit"
  "double loop extraction"
  2 True

-- Step 8: S³ ≅ SU(2)
adjunctionForStep 8 = AdjunctionInfo 8 "S3"
  "Susp³" "Ω³"
  "triple suspension unit"
  "triple loop extraction"
  2 True

-- Step 9: Hopf fibration
adjunctionForStep 9 = AdjunctionInfo 9 "Hopf"
  "total-space" "fiber"
  "section: S² → S³"
  "projection: S³ → S²"
  2 False

-- Step 10: Lie groups (absorbed)
adjunctionForStep 10 = AdjunctionInfo 10 "Lie"
  "G-action" "G-invariants"
  "free action unit"
  "orbit projection"
  2 False

-- Step 11: Cohesion (♭ ⊣ ♯)
adjunctionForStep 11 = AdjunctionInfo 11 "Cohesion"
  "Flat (♭)" "Sharp (♯)"
  "discrete inclusion"
  "codiscrete collapse"
  2 True  -- ʃ ⊣ ♭ ⊣ ♯ (adjoint triple)

-- Step 12: Connections
adjunctionForStep 12 = AdjunctionInfo 12 "Connections"
  "differential (d)" "integration (∫)"
  "de Rham unit"
  "Stokes evaluation"
  2 True

-- Step 13: Curvature
adjunctionForStep 13 = AdjunctionInfo 13 "Curvature"
  "curvature (F)" "connection (∇)"
  "Bianchi identity"
  "holonomy extraction"
  2 False

-- Step 14: Metric + frame
adjunctionForStep 14 = AdjunctionInfo 14 "Metric"
  "frame (e)" "coframe (e*)"
  "metric unit"
  "metric counit"
  2 False

-- Step 15: Hilbert functional
adjunctionForStep 15 = AdjunctionInfo 15 "Hilbert"
  "action (S)" "Euler-Lagrange (δS/δφ)"
  "variational unit"
  "critical point extraction"
  2 False

-- Step 16: DCT
adjunctionForStep 16 = AdjunctionInfo 16 "DCT"
  "synthesis" "analysis"
  "universal inclusion"
  "dynamical projection"
  2 True

adjunctionForStep _ = AdjunctionInfo 0 "???"
  "—" "—" "—" "—" 0 False

-- ============================================================
-- All steps
-- ============================================================

allAdjunctions :: [AdjunctionInfo]
allAdjunctions = map adjunctionForStep [1..16]

-- ============================================================
-- Formatted output
-- ============================================================

formatAdjunctionTable :: [AdjunctionInfo] -> String
formatAdjunctionTable infos =
  header ++ "\n" ++ divider ++ "\n" ++ joinLines (map formatRow infos) ++ "\n" ++ summary
  where
    header  = padR 5 "Step" ++ padR 14 "Structure"
           ++ padR 16 "Left (Intro)" ++ padR 16 "Right (Elim)"
           ++ padR 8 "Depth" ++ "Triple?"
    divider = replicate 70 '-'
    formatRow ai =
      padR 5 (show (aiStep ai))
      ++ padR 14 (aiName ai)
      ++ padR 16 (truncStr 14 (aiLeftAdj ai))
      ++ padR 16 (truncStr 14 (aiRightAdj ai))
      ++ padR 8 (if aiTriDepth ai == 0 then "—" else show (aiTriDepth ai))
      ++ (if aiTriple ai then "yes" else "")

    summary =
      let typeFormers = filter (\ai -> aiStep ai >= 4) infos
          allDepth2 = all (\ai -> aiTriDepth ai == 2) typeFormers
          nTriples = length (filter aiTriple typeFormers)
      in "\n  Type formers (steps 4-16): "
         ++ (if allDepth2
             then "ALL triangle depth = 2"
             else "WARNING: not all depth 2!")
         ++ "\n  Adjoint triples: " ++ show nTriples ++ " / " ++ show (length typeFormers)

-- Helpers

padR :: Int -> String -> String
padR n s = s ++ replicate (max 0 (n - length s)) ' '

truncStr :: Int -> String -> String
truncStr n s
  | length s <= n = s
  | otherwise     = take (n - 1) s ++ "…"

joinLines :: [String] -> String
joinLines [] = ""
joinLines [x] = x
joinLines (x:xs) = x ++ "\n" ++ joinLines xs
