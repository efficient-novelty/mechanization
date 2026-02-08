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
import Enumerate (allProgramsGated)
import qualified Data.Set as Set

-- ============================================
-- Candidate type
-- ============================================

data Candidate
  = CFoundation String     -- Universe, Unit, Witness (steps 1-3)
  | CFormer TypeFormer     -- Pi/Sigma, Trunc
  | CHIT HITDef            -- Any enumerated HIT
  | CSusp String           -- Susp(X) for X in library
  deriving (Eq, Show)

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
candidateName (CSusp base)    = "Susp(" ++ base ++ ")"

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
candidateToEntry (CFormer FSigma)         = LibraryEntry "Pi" 0 [] False Nothing  -- same entry
candidateToEntry (CFormer FTrunc)         = LibraryEntry "Trunc" 0 [] False Nothing
candidateToEntry (CFormer _)              = LibraryEntry "Former" 0 [] False Nothing
candidateToEntry (CHIT h) =
  let name = case knownHITName h of
               Just n  -> n
               Nothing -> "HIT" ++ show (hitNumPoints h) ++ "_" ++ show (map psDimension (hitPaths h))
  in hitToLibraryEntry h name
candidateToEntry (CSusp base) =
  -- Suspension inherits loop property and increments dimension
  LibraryEntry ("Susp_" ++ base) 1 [] True Nothing

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
-- HITs: use hitCost
candidateKappa (CHIT h) _ = hitCost h
-- Suspensions: 1 + kappa of base via program enumeration
candidateKappa (CSusp baseName) ts =
  let lib = tsLibrary ts
      -- Find the minimum program cost for base in the library
      baseKappa = findBaseKappa baseName lib
  in 1 + baseKappa

-- | Find the kappa of a named type in the library.
-- Uses gated program enumeration up to horizon 5.
findBaseKappa :: String -> Library -> Int
findBaseKappa baseName lib =
  let progs = allProgramsGated lib 5
      matching = [programCost p | p <- progs,
                  programToExpr p == TRef baseName]
  in case matching of
       [] -> 1  -- base ref costs 1 if in library
       xs -> minimum xs

-- ============================================
-- Candidate Generation
-- ============================================

-- | Generate all candidates for the current theory state.
--
-- @horizon@ is the admissibility horizon H — only candidates with
-- kappa <= H are generated.
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
        | step < 3  = []  -- Need foundations first
        | otherwise = concat
            [ [CFormer FPi | "Pi" `notElem` libNames
                          , not (Set.member FPi formers) || "Pi" `notElem` libNames]
            , [CFormer FTrunc | "Trunc" `notElem` libNames
                             , Set.member FPi formers]  -- need Pi first
            ]

      -- HIT candidates (only when FHIT is available, step >= 3)
      hitCands
        | step < 3  = []
        | otherwise =
            let hits = enumerateHITs horizon
                -- Filter out HITs that are already in the library
                novel = filter (not . hitInLibrary lib) hits
                -- Filter to only those with loops (interesting HITs)
                loopy = filter hitHasLoop novel
                -- Also include non-loopy if they have points (like Bool)
                withPoints = filter (\h -> hitNumPoints h > 0) novel
            in map CHIT (nubHITs (loopy ++ withPoints))

      -- Suspension candidates (only when we have loopy types)
      suspCands
        | step < 4  = []
        | not (Set.member FSusp formers) = []
        | otherwise =
            [CSusp name | entry <- lib
                        , leHasLoop entry
                        , let name = leName entry
                        , ("Susp_" ++ name) `notElem` libNames]

      allCands = foundations ++ formerCands ++ hitCands ++ suspCands

  in filter (\c -> candidateKappa c ts <= horizon) allCands

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
