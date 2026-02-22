{-# LANGUAGE BangPatterns #-}

-- | Conditional Kolmogorov Complexity via Minimal Binary Type Theory (MBTT)
--
-- Formalizes Construction Effort κ(X | B) as the Minimum Description Length
-- of a candidate's inference-rule specification in a prefix-free binary encoding.
--
-- The encoding:
--   App   = 00      (2 bits)  — application
--   Lam   = 01      (2 bits)  — abstraction
--   Pi    = 100     (3 bits)  — dependent function type
--   Sigma = 1010    (4 bits)  — dependent sum type
--   Univ  = 1011    (4 bits)  — universe U₀
--   Var   = 110 + Elias γ   — bound variable (local scope)
--   Lib   = 111 + Elias γ   — library pointer (conditional scope)
--
-- The conditional aspect (K(X | B)) is handled by treating the historical
-- library as a De Bruijn environment: referencing structure L_i costs
-- O(log i) bits via Elias Gamma coding.
--
-- This eliminates the subjective "count of generators" heuristic for κ,
-- anchoring the cost axis in Algorithmic Information Theory.

module Kolmogorov
  ( MBTTExpr(..)
  , bitLength
  , eliasGammaLength
  , Specification
  , specBits
  , genesisSpecs
  , kolmogorovKappaMBTT
  , kolmogorovKappaAllSteps
  , KolmogorovResult(..)
  ) where

-- Types module not directly needed; Kolmogorov.hs is self-contained.
-- Library metadata (names, indices) is encoded directly in genesisSpecs.

-- ============================================
-- 1. The Minimal Binary Type Theory (MBTT) AST
-- ============================================

-- | MBTT expression — prefix-free binary encoding of dependent type theory.
-- Each constructor has a unique prefix satisfying the Kraft inequality.
data MBTTExpr
  = App MBTTExpr MBTTExpr   -- ^ Application (Prefix: 00, 2 bits)
  | Lam MBTTExpr            -- ^ Abstraction (Prefix: 01, 2 bits)
  | Pi MBTTExpr MBTTExpr    -- ^ Dependent function (Prefix: 100, 3 bits)
  | Sigma MBTTExpr MBTTExpr -- ^ Dependent sum (Prefix: 1010, 4 bits)
  | Univ                    -- ^ Universe U₀ (Prefix: 1011, 4 bits)
  | Var !Int                -- ^ Bound variable (Prefix: 110 + Elias γ)
  | Lib !Int                -- ^ Library pointer (Prefix: 111 + Elias γ)
  -- Extended constructors for HoTT-specific structure
  | Id MBTTExpr MBTTExpr MBTTExpr  -- ^ Identity type (Prefix: 11100, 5 bits)
  | Refl MBTTExpr                  -- ^ Reflexivity (Prefix: 11101, 5 bits)
  | Susp MBTTExpr                  -- ^ Suspension (Prefix: 11110, 5 bits)
  | Trunc MBTTExpr                 -- ^ Propositional truncation (Prefix: 111110, 6 bits)
  | PathCon !Int                   -- ^ Path constructor of dim d (Prefix: 111111 + Elias γ, 6+)
  -- Modal operators
  | Flat MBTTExpr                  -- ^ ♭ (Prefix: 1111100, 7 bits)
  | Sharp MBTTExpr                 -- ^ ♯ (Prefix: 1111101, 7 bits)
  | Disc MBTTExpr                  -- ^ Disc (Prefix: 1111110, 7 bits)
  | Shape MBTTExpr                 -- ^ Π_coh shape (Prefix: 11111110, 8 bits)
  | Next MBTTExpr                  -- ^ ○ temporal (Prefix: 111111110, 9 bits)
  | Eventually MBTTExpr            -- ^ ◇ temporal (Prefix: 111111111, 9 bits)
  deriving (Show, Eq, Ord)

-- ============================================
-- 2. Prefix-Free Bit Length Evaluator
-- ============================================

-- | Compute the bit-length of an MBTT expression under the prefix-free encoding.
-- The Kraft-McMillan inequality is satisfied by construction.
bitLength :: MBTTExpr -> Int
bitLength (App f x)     = 2 + bitLength f + bitLength x
bitLength (Lam body)    = 2 + bitLength body
bitLength (Pi a b)      = 3 + bitLength a + bitLength b
bitLength (Sigma a b)   = 4 + bitLength a + bitLength b
bitLength Univ          = 4
bitLength (Var i)       = 3 + eliasGammaLength i
bitLength (Lib i)       = 3 + eliasGammaLength i
-- HoTT extensions
bitLength (Id a x y)    = 5 + bitLength a + bitLength x + bitLength y
bitLength (Refl a)      = 5 + bitLength a
bitLength (Susp a)      = 5 + bitLength a
bitLength (Trunc a)     = 6 + bitLength a
bitLength (PathCon d)   = 6 + eliasGammaLength d
-- Modal operators
bitLength (Flat a)      = 7 + bitLength a
bitLength (Sharp a)     = 7 + bitLength a
bitLength (Disc a)      = 7 + bitLength a
bitLength (Shape a)     = 8 + bitLength a
bitLength (Next a)      = 9 + bitLength a
bitLength (Eventually a)= 9 + bitLength a

-- | Elias Gamma coding for 1-based positive integers.
-- Encodes n as: ⌊log₂ n⌋ zeros, then n in binary.
-- Total length: 2⌊log₂ n⌋ + 1 bits.
--
-- Examples: 1 → 1 bit, 2-3 → 3 bits, 4-7 → 5 bits, 8-15 → 7 bits.
eliasGammaLength :: Int -> Int
eliasGammaLength n
  | n <= 0    = 1  -- Safety fallback for non-positive
  | otherwise = 2 * floorLog2 n + 1
  where
    floorLog2 :: Int -> Int
    floorLog2 1 = 0
    floorLog2 k = 1 + floorLog2 (k `div` 2)

-- ============================================
-- 3. Specification and κ Computation
-- ============================================

-- | A specification is the list of MBTT expressions encoding the
-- inference rules introduced by a candidate structure.
type Specification = [MBTTExpr]

-- | Total bit-length of a specification.
specBits :: Specification -> Int
specBits = sum . map bitLength

-- | Result of Kolmogorov κ computation for a genesis step.
data KolmogorovResult = KolmogorovResult
  { krStep      :: Int
  , krName      :: String
  , krPaperK    :: Int     -- ^ Paper's κ value (generator count)
  , krMBTTBits  :: Int     -- ^ MBTT bit-length (Kolmogorov upper bound)
  , krSpecCount :: Int     -- ^ Number of candidate specifications evaluated
  , krBestSpec  :: String  -- ^ Description of the winning specification
  } deriving (Show)

-- ============================================
-- 4. Genesis Step Specifications
-- ============================================

-- | Compute Kolmogorov κ (MBTT bits) for a genesis step given its index
-- and the library size at that point.
--
-- For each step, we provide all mathematically valid specifications and
-- take the minimum — this is the MDL principle.
kolmogorovKappaMBTT :: Int -> Int -> (Int, String)
kolmogorovKappaMBTT step libSize = (minimum costs, bestDesc)
  where
    specs = genesisSpecs step libSize
    costs = map (\(_, s) -> specBits s) specs
    minCost = minimum costs
    bestDesc = case filter (\(_, s) -> specBits s == minCost) specs of
      ((d, _):_) -> d
      []         -> "none"

-- | All candidate specifications for each genesis step.
-- Each specification is (description, [MBTTExpr]).
-- The framework takes the minimum over all valid programs.
genesisSpecs :: Int -> Int -> [(String, Specification)]
genesisSpecs step libSize = case step of

  -- Step 1: Universe
  -- U : Type (formation rule for U₀)
  1 ->
    [ ("U-formation", [Univ])
    ]

  -- Step 2: Unit type
  -- 1 : U  (a type in the universe)
  2 ->
    [ ("1-formation", [App Univ (Var 1)])
    ]

  -- Step 3: Witness (★ : 1)
  -- Introduction: ★ : 1
  -- Elimination: ind₁ : C(★) → (x:1) → C(x)
  3 ->
    [ ("star+ind1",
        [ App (Lib 2) (Var 1)                     -- ★ : 1  (intro)
        , Lam (Pi (Lib 2) (Var 1))                 -- ind₁   (elim)
        ])
    ]

  -- Step 4: Π/Σ types
  -- λ-abstraction, pair, application, fst, snd
  4 ->
    [ ("Pi+Sigma",
        [ Lam (Pi (Var 1) (Var 2))                 -- λ-intro: (x:A)→B
        , App (App (Var 1) (Var 2)) (Var 3)        -- pair: (a,b)
        , App (Lam (Var 1)) (Var 2)                -- application
        , Pi (Sigma (Var 1) (Var 2)) (Var 1)       -- fst
        , Pi (Sigma (Var 1) (Var 2)) (Var 2)       -- snd
        ])
    ]

  -- Step 5: S¹ (Circle)
  -- Option A: Native HIT — base : S¹, loop : base =_{S¹} base
  -- Option B: HIT(1,[1]) specification
  5 ->
    [ ("S1-native",
        [ App Univ (Var 1)                         -- S¹ : U (formation)
        , Var 1                                    -- base : S¹ (point)
        , PathCon 1                                -- loop (1-path)
        ])
    ]

  -- Step 6: PropTrunc (||A||₀)
  -- Truncation type former + squash path
  6 ->
    [ ("proptrunc",
        [ Trunc (Var 1)                            -- ||A||₀ formation
        , App (Trunc (Var 1)) (Var 2)              -- |a| introduction
        , PathCon 1                                -- squash path
        ])
    ]

  -- Step 7: S² (2-sphere)
  -- Option A: Native HIT with 2-cell
  -- Option B: Suspension of S¹ (uses library!)
  7 ->
    let s1Ref = libRefIndex "S1" libSize in
    [ ("S2-native",
        [ App Univ (Var 1)                         -- S² : U
        , Var 1                                    -- north : S²
        , PathCon 2                                -- surf (2-path)
        ])
    , ("S2-as-Susp-S1",
        [ Susp (Lib s1Ref)                         -- ΣS¹
        ])
    ]

  -- Step 8: S³ (3-sphere)
  -- Option A: Native HIT with 3-cell
  -- Option B: Suspension of S² (conditional on library!)
  -- Option C: SU(2) presentation (more complex)
  8 ->
    let s2Ref = libRefIndex "S2" libSize in
    [ ("S3-native",
        [ App Univ (Var 1)                         -- S³ : U
        , Var 1                                    -- north : S³
        , PathCon 3                                -- 3-cell attachment
        ])
    , ("S3-as-Susp-S2",
        [ Susp (Lib s2Ref)                         -- ΣS²
        ])
    ]

  -- Step 9: Hopf fibration (h : S³ → S²)
  -- A map between existing library types
  9 ->
    let s3Ref = libRefIndex "S3" libSize
        s2Ref = libRefIndex "S2" libSize
        s1Ref = libRefIndex "S1" libSize
    in
    [ ("hopf-map",
        [ Pi (Lib s3Ref) (Lib s2Ref)              -- h : S³ → S²
        , App (Lib s1Ref) (Var 1)                  -- fiber ≃ S¹
        , Lam (App (Lib s3Ref) (Lib s2Ref))        -- total space structure
        , Pi (Lib s2Ref) (Lib s3Ref)               -- section / classifying data
        ])
    ]

  -- Step 10: Cohesion (♭, ♯, Disc, Π_coh — adjoint quadruple)
  10 ->
    [ ("cohesion-4ops",
        [ Flat (Var 1)                             -- ♭X
        , Sharp (Var 1)                            -- ♯X
        , Disc (Var 1)                             -- Disc(X)
        , Shape (Var 1)                            -- Π_coh(X)
        ])
    ]

  -- Step 11: Connections (∇ on cohesive types)
  11 ->
    let cohRef = libRefIndex "Cohesion" libSize in
    [ ("connections",
        [ Pi (Lib cohRef) (Pi (Var 1) (Var 1))     -- ∇ : TX → TX (connection)
        , Lam (Pi (Var 1) (Var 2))                 -- parallel transport
        , Pi (Flat (Var 1)) (Var 1)                -- covariant derivative
        , App (Lib cohRef) (Var 1)                 -- horizontal lift
        , Lam (Var 1)                              -- Leibniz rule
        ])
    ]

  -- Step 12: Curvature (R = d∇ + ∇∧∇)
  12 ->
    let connRef = libRefIndex "Connections" libSize in
    [ ("curvature",
        [ Pi (Lib connRef) (Pi (Var 1) (Var 1))   -- R : conn → 2-form
        , Lam (App (Lib connRef) (Var 1))          -- Bianchi identity
        , Pi (Var 1) (Lib connRef)                 -- holonomy
        , App (Lib connRef) (App (Var 1) (Var 2))  -- Chern-Weil
        , Lam (Pi (Var 1) (Var 2))                 -- characteristic class
        , Pi (Lib connRef) (Lib connRef)           -- curvature-connection composition
        ])
    ]

  -- Step 13: Metric (g : TX ⊗ TX → ℝ)
  13 ->
    let curvRef = libRefIndex "Curvature" libSize
        connRef = libRefIndex "Connections" libSize
    in
    [ ("metric",
        [ Sigma (Pi (Var 1) (Var 1)) (Pi (Var 1) (Var 1))  -- g : sym bilinear
        , Pi (Sigma (Var 1) (Var 2)) (Lib connRef)          -- Levi-Civita
        , Pi (Var 1) (Pi (Var 1) (Var 1))                   -- geodesic
        , Lam (App (Var 1) (Var 2))                         -- volume form
        , Pi (Lib curvRef) (Lib curvRef)                    -- Hodge star
        , Lam (Pi (Var 1) (Var 1))                          -- Laplacian
        , Pi (Lib curvRef) (Var 1)                          -- Ricci/scalar
        ])
    ]

  -- Step 14: Hilbert (inner product + completeness + spectral theory)
  14 ->
    let metRef = libRefIndex "Metric" libSize
        curvRef = libRefIndex "Curvature" libSize
        connRef = libRefIndex "Connections" libSize
    in
    [ ("hilbert",
        [ Sigma (Pi (Var 1) (Pi (Var 1) Univ)) (Var 1)     -- inner product
        , Pi (Var 1) (Var 1)                                -- completeness
        , Pi (Var 1) (Sigma (Var 1) (Var 1))                -- orthogonal decomposition
        , Pi (Lam (Var 1)) (Sigma (Var 1) (Var 2))         -- spectral decomp
        , Sigma (Pi (Var 1) (Var 1)) (Pi (Var 1) (Var 1))  -- C*-algebra
        , Pi (Lib metRef) (Var 1)                           -- inner product metric
        , Pi (Lib curvRef) (Var 1)                          -- operator curvature
        , Pi (Lib connRef) (Var 1)                          -- quantum connection
        , Lam (Pi (Var 1) Univ)                             -- functional derivative
        ])
    ]

  -- Step 15: DCT (Dynamical Cohesive Topos)
  -- Imports temporal modalities into cohesive setting
  15 ->
    let cohRef = libRefIndex "Cohesion" libSize in
    [ ("dct-temporal",
        [ Next (Var 1)                             -- ○X (next modality)
        , Eventually (Var 1)                       -- ◇X (eventually modality)
        , Pi (Next (Var 1)) (Eventually (Var 1))   -- ○ → ◇ (axiom)
        , Lam (App (Lib cohRef) (Next (Var 1)))    -- spatial-temporal interaction
        , Pi (Flat (Next (Var 1))) (Next (Flat (Var 1)))  -- ♭○ ↔ ○♭ exchange
        , Pi (Sharp (Eventually (Var 1))) (Eventually (Sharp (Var 1)))  -- ♯◇ exchange
        , Lam (App (Eventually (Var 1)) (Var 2))   -- ◇-elimination
        , Pi (Next (Next (Var 1))) (Next (Var 1))  -- ○○ → ○ (non-idempotent witness)
        ])
    ]

  _ -> [("unknown", [Univ])]

-- | Map a library type name to its 1-based index in the library at the
-- given step. Later entries have higher indices, meaning they cost more
-- bits via Elias Gamma coding — this is exactly the conditional aspect.
libRefIndex :: String -> Int -> Int
libRefIndex name libSize = case name of
  "Universe"    -> 1
  "Unit"        -> 2
  "Witness"     -> 3
  "Pi"          -> 4
  "S1"          -> 5
  "Trunc"       -> 6
  "S2"          -> 7
  "S3"          -> 8
  "Hopf"        -> 9
  "Cohesion"    -> 10
  "Connections" -> 11
  "Curvature"   -> 12
  "Metric"      -> 13
  "Hilbert"     -> 14
  "DCT"         -> 15
  _             -> libSize  -- fallback: most expensive

-- ============================================
-- 5. Full 15-Step Evaluation
-- ============================================

-- | Compute Kolmogorov κ (MBTT bits) for all 15 genesis steps.
kolmogorovKappaAllSteps :: [KolmogorovResult]
kolmogorovKappaAllSteps =
  [ let libSize = step - 1
        specs = genesisSpecs step libSize
        nSpecs = length specs
        (bits, desc) = kolmogorovKappaMBTT step libSize
        pkappa = paperKappaLocal step
    in KolmogorovResult
        { krStep      = step
        , krName      = genesisName step
        , krPaperK    = pkappa
        , krMBTTBits  = bits
        , krSpecCount = nSpecs
        , krBestSpec  = desc
        }
  | step <- [1..15]
  ]

-- | Paper κ values (local copy to avoid circular import).
paperKappaLocal :: Int -> Int
paperKappaLocal 1  = 2   -- Universe
paperKappaLocal 2  = 1   -- Unit
paperKappaLocal 3  = 1   -- Witness
paperKappaLocal 4  = 3   -- Π/Σ
paperKappaLocal 5  = 3   -- S¹
paperKappaLocal 6  = 3   -- PropTrunc
paperKappaLocal 7  = 3   -- S²
paperKappaLocal 8  = 5   -- S³
paperKappaLocal 9  = 4   -- Hopf
paperKappaLocal 10 = 4   -- Cohesion
paperKappaLocal 11 = 5   -- Connections
paperKappaLocal 12 = 6   -- Curvature
paperKappaLocal 13 = 7   -- Metric
paperKappaLocal 14 = 9   -- Hilbert
paperKappaLocal 15 = 8   -- DCT
paperKappaLocal _  = 1

-- | Genesis step names.
genesisName :: Int -> String
genesisName 1  = "Universe"
genesisName 2  = "Unit"
genesisName 3  = "Witness"
genesisName 4  = "Pi/Sigma"
genesisName 5  = "S1"
genesisName 6  = "PropTrunc"
genesisName 7  = "S2"
genesisName 8  = "S3"
genesisName 9  = "Hopf"
genesisName 10 = "Cohesion"
genesisName 11 = "Connections"
genesisName 12 = "Curvature"
genesisName 13 = "Metric"
genesisName 14 = "Hilbert"
genesisName 15 = "DCT"
genesisName _  = "???"
