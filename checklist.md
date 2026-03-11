# Strict Intelligence Implementation Checklist

This checklist turns the current plan into a concrete patch sequence against the code as it exists now.

Primary target:

- steps 1-5 in about 60s total
- steps 6-10 in about 5m more
- steps 11-14 in about 10m more
- step 15 in about 10m more
- no regression into seeds, representatives, canonical-name ranking, or goal-family steering

Non-negotiables:

- `--strict` stays seed-free and representative-free
- strict scoring stays canonical-name-neutral
- strict selection stays name-free
- admissibility must be explained by structural debt, not by target-family labels
- MCTS may become guided only by obligations inferred from failed strict frontiers

## Recommended Patch Order

Patch in this order so the tree stays buildable and each milestone is measurable.

1. Add `engine/src/StrictCritic.hs`
2. Add `engine/src/StrictMinimality.hs`
3. Update `engine/pen-engine.cabal` to expose the new modules
4. Update `engine/src/MBTTEnum.hs` to use critic-guided frontier retention and emit diagnostics
5. Update `engine/src/RunAbInitio.hs` for strict frontier telemetry, honest ranking rewrite, and SCC minimality
6. Update `engine/src/AbInitioPolicy.hs` to derive caps from interface debt instead of family-like capability stages
7. Update `engine/src/MCTS.hs` to seed from failed frontiers and derive dynamic rollout priors from obligations
8. Update `engine/src/RunAbInitio.hs` again to pass MCTS seeds / diagnostics through the honest lane
9. Update `engine/src/AcceptanceSuite.hs` with phase-by-phase tests
10. Run benchmark gates and tune only critic weights / cap constants, not search breadth globally

Build gate after patches 1-3:

```powershell
cabal build lib:pen-engine
```

Build gate after patches 4-8:

```powershell
cabal build ab-initio acceptance-core
```

Test gate after patch 9:

```powershell
cabal run acceptance-core
```

Benchmark gate after each milestone:

```powershell
cabal run ab-initio -- --strict --max-steps 5
cabal run ab-initio -- --strict --max-steps 10
cabal run ab-initio -- --strict --max-steps 14
cabal run ab-initio -- --strict
```

## File-By-File Checklist

### 1. `engine/src/StrictCritic.hs` (new)

Purpose:

- hold all strict-only mathematical-intelligence logic that is allowed before final truth evaluation
- keep obligation analysis structural and name-free
- make the same obligation vocabulary available to enumeration, admissibility, ranking, and MCTS

Add these core types first:

```haskell
data MissingSignature
  = NeedIntro !Int
  | NeedElim !Int
  | NeedTransport !Int
  | NeedCoherence !Int
  | NeedBridge ![Int]
  deriving (Show, Eq, Ord)

data ObligationSummary = ObligationSummary
  { osScore :: !Int
  , osAdjointDebt :: !Int
  , osFloatingClauses :: !Int
  , osInterfaceDensity :: !Int
  , osGenericBinderCount :: !Int
  , osClosureScore :: !Int
  , osMissingSignatures :: ![MissingSignature]
  } deriving (Show, Eq)

data FrontierDiagnostics = FrontierDiagnostics
  { fdFrontierCandidates :: !Int
  , fdFrontierKept :: !Int
  , fdBestObligationScore :: !(Maybe Int)
  , fdBestInterfaceDensity :: !(Maybe Int)
  , fdBestGenericityScore :: !(Maybe Int)
  , fdBestClosureScore :: !(Maybe Int)
  } deriving (Show, Eq)
```

Add these functions:

```haskell
analyzeObligations :: Library -> Telescope -> ObligationSummary
interfaceDensity :: Telescope -> Int
genericBinderCount :: Telescope -> Int
closureScore :: Library -> Telescope -> Int
missingGoalProfile :: ObligationSummary -> GoalProfile
frontierDiagnostics :: [ObligationSummary] -> FrontierDiagnostics
```

Implementation checklist:

- derive obligation features only from telescope structure, binder structure, and library references
- count formation / intro / elim balance without looking at canonical names
- penalize floating clauses and disconnected leaves
- reward recent-library interface reuse
- keep all integer weights local and auditable in this module
- keep `missingGoalProfile` coarse and structural; it is for MCTS priors only

Definition of done:

- `analyzeObligations` is name-invariant
- balanced formers outscore disconnected junk
- the module has no dependency on canonical-name decoding or reference telescopes

### 2. `engine/src/StrictMinimality.hs` (new)

Purpose:

- replace one-entry deletion with SCC-based semantic minimality
- keep minimality polynomial-time and reusable from `RunAbInitio`

Add these types and functions:

```haskell
type ClauseIx = Int
type ClauseGraph = Map ClauseIx (Set ClauseIx)

buildClauseDependencyGraph :: Telescope -> ClauseGraph
terminalClauseSCCs :: Telescope -> [[ClauseIx]]
removeClauseSet :: [ClauseIx] -> Telescope -> Telescope
terminalSCCSubBundles :: Telescope -> [Telescope]
```

Implementation checklist:

- node = telescope entry index
- edge `i -> j` iff entry `i` references entry `j` in type/body structure
- use Tarjan or Kosaraju; graphs are tiny
- return only terminal SCCs, not all SCCs
- make `terminalSCCSubBundles` deterministic in index order

Definition of done:

- packed candidates produce at least one nontrivial SCC amputation
- simple prime candidates often produce no admissible amputation

### 3. `engine/pen-engine.cabal`

Purpose:

- expose the new modules to `ab-initio` and acceptance suites

Checklist:

- add `StrictCritic` and `StrictMinimality` to the library exposed modules
- add both modules to `ab-initio`, `acceptance`, `acceptance-core`, and `acceptance-mbtt`
- keep module ordering stable and minimal

Definition of done:

- `cabal build lib:pen-engine` succeeds before any call-site rewiring

### 4. `engine/src/MBTTEnum.hs`

Purpose:

- move strict frontier retention from syntax-blind truncation to obligation-guided truncation
- expose per-band frontier diagnostics to the strict controller

Preferred API additions:

```haskell
data FrontierMode
  = FrontierNeutral
  | FrontierGoalDirected
  | FrontierObligationGuided
  deriving (Show, Eq)

data EnumBandDiagnostics = EnumBandDiagnostics
  { ebdFrontier :: !FrontierDiagnostics
  , ebdExprCountsByCtx :: ![Int]
  , ebdScannedTelescopes :: !Int
  , ebdValidTelescopes :: !Int
  } deriving (Show, Eq)

enumerateMBTTTelescopesWithDiagnostics
  :: Library
  -> EnumConfig
  -> EnumStepCache
  -> (([MBTTCandidate], EnumBandDiagnostics), EnumStepCache)
```

Recommended `EnumConfig` changes:

```haskell
data EnumConfig = EnumConfig
  { ecMaxBitBudget :: !Int
  , ecMaxEntries :: !Int
  , ecMaxASTDepth :: !Int
  , ecMaxCandidates :: !Int
  , ecExactClauseCount :: !(Maybe Int)
  , ecExprCapLimit :: !(Maybe Int)
  , ecGoalProfile :: !(Maybe GoalProfile)
  , ecEnableMacros :: !Bool
  , ecFrontierMode :: !FrontierMode
  }
```

Current field to retire:

```haskell
ecNeutralHeuristic :: !Bool
```

Replace or adapt these internals:

```haskell
candidateHeuristic :: Bool -> Telescope -> Library -> GoalProfile -> Int
candidateFrontierKey
  :: EnumConfig
  -> Library
  -> GoalProfile
  -> Telescope
  -> (Down Int, Int, Int, Int, String)
```

Implementation checklist:

- keep `enumerateMBTTTelescopesWithCache` as a wrapper for compatibility
- add `enumerateMBTTTelescopesWithDiagnostics` as the new strict entry point
- in `FrontierObligationGuided` mode, compute `ObligationSummary` for each retained telescope
- sort frontier by obligation score, then lower bit cost, then lower clause count, then canonical structural key
- record `FrontierDiagnostics` for the band
- do not re-enable macros or goal-family steering in strict mode

Definition of done:

- strict step 4 surfaces materially better `kappa = 3` candidates without any seeds
- guided recovery behavior is unchanged when `FrontierGoalDirected` is used

### 5. `engine/src/RunAbInitio.hs` (phase A: strict telemetry, ranking, minimality)

Purpose:

- consume band diagnostics
- replace single-entry minimality with SCC minimality
- replace taxonomy tie-breaks with density / genericity / closure tie-breaks

Add or revise these types:

```haskell
data HonestBandTrace = HonestBandTrace
  { hbtBand :: !Int
  , hbtElapsedSeconds :: !Double
  , hbtFrontier :: !FrontierDiagnostics
  , hbtEvaluated :: !Int
  , hbtDeduped :: !Int
  , hbtViable :: !Int
  } deriving (Show)

data HonestSearchState = HonestSearchState
  { hsEnumCache :: !EnumStepCache
  , hsEvalCache :: !EvalCache
  , hsSeenKeys :: !(Set.Set CanonKey)
  , hsRawCandidates :: !Int
  , hsEvaluatedCandidates :: !Int
  , hsDedupedCandidates :: !Int
  , hsMinimalityRejects :: !Int
  , hsSearchedBands :: ![Int]
  , hsViableCandidates :: ![Candidate]
  , hsBandTraces :: ![HonestBandTrace]
  , hsFailedFrontier :: ![(Telescope, ObligationSummary)]
  }
```

Replace the current selection key:

```haskell
honestSelectionKey
  :: Library
  -> Double
  -> Candidate
  -> (Double, Int, Down Int, Down Int, Down Int, Int, String)
```

Required helpers:

```haskell
countDistinctLibraryRefs :: Telescope -> Int
countPolymorphicBinders :: Telescope -> Int
strictClosureTieBreak :: Library -> Telescope -> Int
stableStructuralHash :: Telescope -> String
```

Replace the current minimality helper:

```haskell
applyStrictSemanticMinimalityFilter
  :: EvalMode
  -> Library
  -> Int
  -> Double
  -> [(Int, Int)]
  -> [Int]
  -> EvalCache
  -> [Candidate]
  -> ([Candidate], EvalCache, Int)
```

Checklist:

- switch honest enumeration from `enumerateMBTTTelescopesWithCache` to `enumerateMBTTTelescopesWithDiagnostics`
- append one `HonestBandTrace` per searched band
- preserve exact `desugaredKappa` band search order
- replace `properSubBundles` usage with `terminalSCCSubBundles`
- keep sub-bundle admissibility checks before re-evaluation
- change `honestSelectionKey` ordering to:
  - minimal positive overshoot
  - lower `kappa`
  - higher interface density
  - higher generic binder count
  - higher closure score
  - lower bit cost
  - stable structural hash
- delete strict dependence on `telescopeClassRank`

Current helpers to delete or demote from strict use:

- `properSubBundles`
- `telescopeClassRank`
- `stableStructuralKey` in its current form

Definition of done:

- strict ranking is deterministic and name-free
- prefix telemetry can explain band failures using critic metrics
- SCC minimality rejects packed candidates the current one-entry filter misses

### 6. `engine/src/AbInitioPolicy.hs`

Purpose:

- replace coarse family-like capability staging with debt-scaled admissibility

Add these types and functions:

```haskell
data InterfaceDebt = InterfaceDebt
  { idActiveInterfaceSize :: !Int
  , idObligationSurface :: !Int
  , idSuggestedCap :: !Int
  , idSuggestedBands :: ![Int]
  } deriving (Show, Eq)

strictInterfaceDebt :: Int -> Library -> InterfaceDebt
countActiveExports :: Int -> Library -> Int
estimateObligationSurface :: Int -> Library -> Int
capFromInterfaceDebt :: InterfaceDebt -> Int
bandsFromInterfaceDebt :: InterfaceDebt -> [Int]
```

Keep this public signature stable:

```haskell
strictAdmissibility :: Int -> Library -> StrictAdmissibility
```

Implementation checklist:

- keep steps 1-3 as explicit bootstrap axioms
- use `d = 2` active window by default
- compute interface size from the most recent discovered layers
- derive obligation surface from unresolved structural debt, not canonical names
- use a narrow band policy:
  - exact band when one minimal cost is strongly indicated
  - short trailing range when closure debt is ambiguous
- keep caps monotone enough that later steps can logically take longer

Definition of done:

- strict no longer branches on `hasMetric`, `hasHilbert`, `hasTemporal`, etc. for its main cap logic
- early strict cannot open broad high-`kappa` search

### 7. `engine/src/MCTS.hs`

Purpose:

- turn late MCTS into a debt-completion engine seeded from failed strict frontiers

Add these types:

```haskell
data SeededFrontierState = SeededFrontierState
  { sfsTelescope :: !Telescope
  , sfsSummary :: !ObligationSummary
  } deriving (Show)

data MCTSConfig = MCTSConfig
  { mctsIterations :: !Int
  , mctsMaxKappa :: !Int
  , mctsMaxDepth :: !Int
  , mctsExploreC :: !Double
  , mctsNuDepth :: !Int
  , mctsTopK :: !Int
  , mctsSeed :: !Int
  , mctsVerbose :: !Bool
  , mctsWidenC :: !Double
  , mctsWidenAlpha :: !Double
  , mctsGoalProfile :: !(Maybe GoalProfile)
  , mctsEnableMacroReuse :: !Bool
  , mctsSeedFrontier :: ![SeededFrontierState]
  }
```

Add these helpers:

```haskell
dynamicGoalProfileFromSummary :: ObligationSummary -> GoalProfile
actionPriorWeight :: ObligationSummary -> Action -> Double
seededRootNodes :: MCTSConfig -> [MCTSNode]
```

Implementation checklist:

- keep the public `mctsSearchStep` signature unchanged if possible
- seed rollouts from the best failed frontier states instead of the empty telescope only
- derive rollout bias from `osMissingSignatures`
- keep macro reuse disabled in strict mode
- keep canonical names and representative telescopes inaccessible from this module

Definition of done:

- MCTS stops flailing randomly in late strict phases
- all MCTS guidance can be audited back to structural obligations

### 8. `engine/src/RunAbInitio.hs` (phase B: MCTS seeding)

Purpose:

- pass failed strict frontier states into MCTS cleanly
- keep the honest lane's allowed source set explicit

Add these helpers:

```haskell
failedFrontierSeeds :: HonestSearchState -> [SeededFrontierState]
recordFailedFrontier :: [MBTTCandidate] -> HonestSearchState -> HonestSearchState
```

Checklist:

- after each strict band, retain a small top slice of highest-obligation failed frontier states
- only keep states from admissible bands
- pass `mctsSeedFrontier` into `defaultMCTSConfig`
- when MCTS returns viable candidates, keep only the minimum-`kappa` viable MCTS band as before
- extend strict telemetry with:
  - `best_obligation_score`
  - `best_interface_density`
  - `best_genericity_score`
  - `best_closure_score`
  - `frontier_candidates`
  - `frontier_kept`
  - `mcts_seed_states`
  - `mcts_completed_states`

Definition of done:

- strict provenance remains auditable
- the only strict candidate sources are typed enumeration, failed-frontier completion MCTS, and honest evaluation

### 9. `engine/src/AcceptanceSuite.hs`

Purpose:

- lock in the anti-cheating contract and the new structural intelligence

Add these tests:

```haskell
testK4StrictCriticNameInvariant :: IO Bool
testK5StrictCriticPrefersBalancedFormer :: IO Bool
testK6StrictMinimalityRejectsPackedBundle :: IO Bool
testK7StrictCapsDerivedFromInterfaceDebt :: IO Bool
testK8HonestRankingPrefersDensityAndGenericity :: IO Bool
testK9StrictMCTSUsesObligationsOnly :: IO Bool
```

Checklist:

- test that `analyzeObligations` does not depend on canonical names
- test that disconnected junk is outranked by balanced former-like bundles
- test that SCC minimality rejects a packed candidate that one-entry deletion would keep
- test that early library states cannot open wide `kappa` caps
- test that honest selection never inspects canonical names
- test that strict MCTS uses generated obligation profiles, not family seeds

Definition of done:

- acceptance suite protects both honesty and performance-oriented intelligence

## Implementation Notes By Milestone

### Milestone A: Critic scaffolding

Files:

- `engine/src/StrictCritic.hs`
- `engine/pen-engine.cabal`
- `engine/src/AcceptanceSuite.hs`

Goal:

- land obligation analysis with tests before any search-control rewiring

### Milestone B: Critic-guided frontier

Files:

- `engine/src/MBTTEnum.hs`
- `engine/src/RunAbInitio.hs`

Goal:

- make exact-band search keep the mathematically promising partials

Success gate:

- strict step 4 becomes viable without seeds

### Milestone C: SCC minimality

Files:

- `engine/src/StrictMinimality.hs`
- `engine/src/RunAbInitio.hs`
- `engine/src/AcceptanceSuite.hs`

Goal:

- reduce packed-candidate waste, especially in later phases

### Milestone D: Endogenous caps

Files:

- `engine/src/AbInitioPolicy.hs`
- `engine/src/AcceptanceSuite.hs`

Goal:

- open `kappa` only when active interface debt demands it

### Milestone E: Honest ranking rewrite

Files:

- `engine/src/RunAbInitio.hs`
- `engine/src/AcceptanceSuite.hs`

Goal:

- remove remaining taxonomy bias in strict selection

### Milestone F: Completion MCTS

Files:

- `engine/src/MCTS.hs`
- `engine/src/RunAbInitio.hs`
- `engine/src/AcceptanceSuite.hs`

Goal:

- complete late structures from unresolved debts instead of canned goals

## Calibration Checklist

Tune only these parameters first:

- obligation score weights in `StrictCritic`
- interface-debt cap constants in `AbInitioPolicy`
- number of retained failed-frontier seed states for MCTS
- UCT prior weight from missing signatures

Do not tune these first:

- global `ecMaxCandidates`
- global AST depth
- global `kappa` cap
- reintroduction of macros, seeds, or representative telescopes

## Final Acceptance Conditions

The implementation is ready for serious review when all of the following are true:

- strict finds the first 5 steps in about 60s warm
- strict finds the first 10 steps in about 6m cumulative
- strict finds steps 11-14 in about 16m cumulative
- DCT is reachable in about 26-30m cumulative
- logs can explain each late discovery in terms of structural debt closure
- no strict code path uses representative matching, family rescue, canonical-name ranking, or seed telescopes
