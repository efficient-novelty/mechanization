-- | Candidate generation for synthesis
--
-- Generates candidate structures at each step of the synthesis loop:
--   - Foundation candidates (Universe, Unit, Witness) for steps 1-3
--   - Type former candidates (Pi/Sigma, PropTrunc) when not yet added
--   - HIT candidates from HITEnum up to cost horizon
--   - Suspension candidates (Susp(X)) for loopy library types

module Generator
  ( Candidate(..)
  , generateCandidates
  , candidateKappa
  , candidateToEntry
  , candidateName
  ) where

import Types
import TheoryState
import HITEnum
import qualified Data.Set as Set

-- ============================================
-- Candidate type
-- ============================================

data Candidate
  = CFoundation String     -- Universe, Unit, Witness (steps 1-3)
  | CFormer TypeFormer     -- Pi/Sigma, Trunc
  | CHIT HITDef            -- Any enumerated HIT
  | CSusp String           -- Susp(X) for X in library
  | CMap String String String   -- source target fiber (e.g., "S3" "S2" "S1")
  | CAlgebra String String      -- kind carrier (e.g., "Lie" "S3")
  | CModal String Int           -- name numOps (e.g., "Cohesion" 3)
  deriving (Eq, Show)

-- ============================================
-- Suspension naming
-- ============================================

-- | Map suspension of known types to proper sphere names
suspensionName :: String -> String
suspensionName "S1" = "S2"
suspensionName "S2" = "S3"
suspensionName b = "Susp(" ++ b ++ ")"

-- ============================================
-- Candidate naming
-- ============================================

-- | Human-readable name for a candidate
candidateName :: Candidate -> String
candidateName (CFoundation s) = s
candidateName (CFormer FPi)   = "Pi/Sigma"
candidateName (CFormer FSigma) = "Pi/Sigma"
candidateName (CFormer FTrunc) = "PropTrunc"
candidateName (CFormer f)     = show f
candidateName (CHIT h)        = case knownHITName h of
                                  Just n  -> n
                                  Nothing -> "HIT" ++ show (hitNumPoints h)
                                          ++ "_" ++ show (map psDimension (hitPaths h))
candidateName (CSusp base)    = suspensionName base
candidateName (CMap _ _ _)    = "Hopf"
candidateName (CAlgebra k _)  = k
candidateName (CModal n _)    = n

-- ============================================
-- Candidate to LibraryEntry
-- ============================================

-- | Convert a candidate to a LibraryEntry for insertion into the library
candidateToEntry :: Candidate -> LibraryEntry
candidateToEntry (CFoundation "Universe") = LibraryEntry "U" 0 [] False Nothing
candidateToEntry (CFoundation "Unit")     = LibraryEntry "1" 1 [] False (Just 0)
candidateToEntry (CFoundation "Witness")  = LibraryEntry "star" 1 [] False Nothing
candidateToEntry (CFoundation s)          = LibraryEntry s 0 [] False Nothing
candidateToEntry (CFormer FPi)            = LibraryEntry "Pi" 0 [] False Nothing
candidateToEntry (CFormer FSigma)         = LibraryEntry "Pi" 0 [] False Nothing
candidateToEntry (CFormer FTrunc)         = LibraryEntry "Trunc" 0 [] False Nothing
candidateToEntry (CFormer _)              = LibraryEntry "Former" 0 [] False Nothing
candidateToEntry (CHIT h) =
  let name = case knownHITName h of
               Just n  -> n
               Nothing -> "HIT" ++ show (hitNumPoints h)
                       ++ "_" ++ show (map psDimension (hitPaths h))
  in hitToLibraryEntry h name
candidateToEntry (CSusp base) =
  -- Suspension produces a sphere when base is a sphere
  case base of
    "S1" -> LibraryEntry "S2" 1 [2] True Nothing
    "S2" -> LibraryEntry "S3" 1 [3] True Nothing
    _    -> LibraryEntry (suspensionName base) 1 [] True Nothing
candidateToEntry (CMap _ _ _) =
  LibraryEntry "Hopf" 0 [] True Nothing
candidateToEntry (CAlgebra kind carrier) =
  LibraryEntry (kind ++ "(" ++ carrier ++ ")") 0 [] False Nothing
candidateToEntry (CModal name _) =
  LibraryEntry name 0 [] False Nothing

-- ============================================
-- Kappa computation
-- ============================================

-- | Compute kappa (Kolmogorov complexity) for a candidate.
candidateKappa :: Candidate -> TheoryState -> Int
-- Foundations: hardcoded (these are axioms)
candidateKappa (CFoundation "Universe") _ = 2
candidateKappa (CFoundation "Unit")     _ = 1
candidateKappa (CFoundation "Witness")  _ = 1
candidateKappa (CFoundation _)          _ = 1
-- Formers: hardcoded
candidateKappa (CFormer FPi)    _ = 3
candidateKappa (CFormer FSigma) _ = 3
candidateKappa (CFormer FTrunc) _ = 3
candidateKappa (CFormer _)      _ = 3
-- HITs: use hitCost, considering suspension shortcuts
candidateKappa (CHIT h) ts = hitKappa h ts
-- Suspensions: north + south (points) + merid (path) = 3 constructors
candidateKappa (CSusp _) _ = 3
-- Maps: fiber + total + base + map construction = 4
candidateKappa (CMap _ _ _) _ = 4
-- Algebras: carrier + 5 axioms = 6
candidateKappa (CAlgebra _ _) _ = 6
-- Modals: 1 + numOps
candidateKappa (CModal _ numOps) _ = 1 + numOps

-- | Compute kappa for a HIT, considering suspension shortcuts.
hitKappa :: HITDef -> TheoryState -> Int
hitKappa h ts =
  let baseCost = hitCost h
      libNames = map leName (tsLibrary ts)
      suspCost = case knownHITName h of
        Just "S2" | "S1" `elem` libNames -> 3
        Just "S3" | "S2" `elem` libNames -> 3
        _ -> baseCost
  in min baseCost suspCost

-- ============================================
-- Candidate Generation
-- ============================================

-- | Generate all candidates for the current theory state.
generateCandidates :: TheoryState -> Int -> [Candidate]
generateCandidates ts horizon =
  let step = tsStep ts
      lib  = tsLibrary ts
      formers = tsFormers ts
      libNames = map leName lib

      -- Foundation candidates (only at steps 0-2)
      foundations
        | step == 0 = [CFoundation "Universe"]
        | step == 1 = [CFoundation "Unit"]
        | step == 2 = [CFoundation "Witness"]
        | otherwise = []

      -- Type former candidates (if not yet in library)
      formerCands
        | step < 3  = []
        | otherwise = concat
            [ [CFormer FPi | "Pi" `notElem` libNames]
            , [CFormer FTrunc | "Trunc" `notElem` libNames
                             , Set.member FPi formers]
            ]

      -- HIT candidates (step >= 3, must have points)
      hitCands
        | step < 3  = []
        | otherwise =
            let hits = enumerateHITs horizon
                novel = filter (not . hitInLibrary lib) hits
                withPoints = filter (\h -> hitNumPoints h > 0) novel
                -- Remove HITs that duplicate suspension candidates
                noSuspDup = filter (not . duplicatesSusp libNames) withPoints
            in map CHIT (nubHITs noSuspDup)

      -- Suspension candidates (when loopy types exist)
      suspCands
        | step < 4  = []
        | not (Set.member FSusp formers) = []
        | otherwise =
            [CSusp name | entry <- lib
                        , leHasLoop entry
                        , let name = leName entry
                        , let sName = suspensionName name
                        , sName `notElem` libNames]

      -- Map candidates (fibrations): require S1, S2, S3 all in library
      mapCands
        | "S1" `elem` libNames && "S2" `elem` libNames && "S3" `elem` libNames
        , "Hopf" `notElem` libNames
        = [CMap "S3" "S2" "S1"]
        | otherwise = []

      -- Algebra candidates: require S3 in library
      algebraCands
        | "S3" `elem` libNames
        , "Lie(S3)" `notElem` libNames
        = [CAlgebra "Lie" "S3"]
        | otherwise = []

      -- Modal candidates: require FFibration former (Hopf realized)
      modalCands
        | Set.member FFibration formers
        , "Cohesion" `notElem` libNames
        = [CModal "Cohesion" 3]
        | otherwise = []

      allCands = foundations ++ formerCands ++ hitCands ++ suspCands
             ++ mapCands ++ algebraCands ++ modalCands

  in filter (\c -> candidateKappa c ts <= horizon) allCands

-- | Check if a HIT duplicates a suspension candidate
duplicatesSusp :: [String] -> HITDef -> Bool
duplicatesSusp libNames h = case knownHITName h of
  Just "S2" -> "S1" `elem` libNames
  Just "S3" -> "S2" `elem` libNames
  _ -> False

-- | Check if a HIT is already represented in the library
hitInLibrary :: Library -> HITDef -> Bool
hitInLibrary lib h =
  let dims = map psDimension (hitPaths h)
      pts = hitNumPoints h
  in any (\e -> leConstructors e == pts && lePathDims e == dims) lib

-- | Remove duplicate HITs
nubHITs :: [HITDef] -> [HITDef]
nubHITs [] = []
nubHITs (h:hs) = h : nubHITs (filter (/= h) hs)
