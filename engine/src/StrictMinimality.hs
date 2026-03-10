module StrictMinimality
  ( ClauseIx
  , ClauseGraph
  , buildClauseDependencyGraph
  , terminalClauseSCCs
  , removeClauseSet
  , terminalSCCSubBundles
  ) where

import Kolmogorov (MBTTExpr(..))
import Telescope (TeleEntry(..), Telescope(..))

import Data.Graph (SCC(..), stronglyConnComp)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type ClauseIx = Int
type ClauseGraph = Map.Map ClauseIx (Set.Set ClauseIx)

buildClauseDependencyGraph :: Telescope -> ClauseGraph
buildClauseDependencyGraph (Telescope entries) =
  Map.fromList
    [ (ix, entryDeps ix 0 (teType entry))
    | (ix, entry) <- zip [0..] entries
    ]

terminalClauseSCCs :: Telescope -> [[ClauseIx]]
terminalClauseSCCs tele@(Telescope entries) =
  let graph = buildClauseDependencyGraph tele
      sccs = stronglyConnComp
        [ (ix, ix, Set.toList deps)
        | (ix, deps) <- Map.toAscList graph
        ]
      ordered =
        [ sortOn id members
        | component <- sccs
        , let members = sccMembers component
        , not (null members)
        , length members < length entries
        ]
  in sortOn componentStart [members | members <- ordered, isTerminal graph members]

removeClauseSet :: [ClauseIx] -> Telescope -> Telescope
removeClauseSet removeIxs (Telescope entries) =
  let removeSet = Set.fromList removeIxs
  in Telescope
       [ entry
       | (ix, entry) <- zip [0..] entries
       , not (Set.member ix removeSet)
       ]

terminalSCCSubBundles :: Telescope -> [Telescope]
terminalSCCSubBundles tele =
  [ amputated
  | component <- terminalClauseSCCs tele
  , let amputated@(Telescope entries) = removeClauseSet component tele
  , not (null entries)
  ]

sccMembers :: SCC ClauseIx -> [ClauseIx]
sccMembers scc = case scc of
  AcyclicSCC v -> [v]
  CyclicSCC vs -> vs

componentStart :: [ClauseIx] -> ClauseIx
componentStart members = case members of
  (ix:_) -> ix
  [] -> 0

isTerminal :: ClauseGraph -> [ClauseIx] -> Bool
isTerminal graph component =
  let members = Set.fromList component
      outgoing =
        Set.unions
          [ Map.findWithDefault Set.empty ix graph
          | ix <- component
          ]
  in Set.null (Set.difference outgoing members)

entryDeps :: ClauseIx -> Int -> MBTTExpr -> Set.Set ClauseIx
entryDeps current binderDepth expr = case expr of
  App f x -> Set.union (entryDeps current binderDepth f) (entryDeps current binderDepth x)
  Lam body -> entryDeps current (binderDepth + 1) body
  Pi a b -> Set.union (entryDeps current binderDepth a) (entryDeps current (binderDepth + 1) b)
  Sigma a b -> Set.union (entryDeps current binderDepth a) (entryDeps current (binderDepth + 1) b)
  Univ -> Set.empty
  Var i ->
    case i - binderDepth of
      shifted
        | shifted >= 1 ->
            let dep = current - shifted
            in if dep >= 0 then Set.singleton dep else Set.empty
      _ -> Set.empty
  Lib _ -> Set.empty
  Id a x y -> Set.unions [entryDeps current binderDepth a, entryDeps current binderDepth x, entryDeps current binderDepth y]
  Refl a -> entryDeps current binderDepth a
  Susp a -> entryDeps current binderDepth a
  Trunc a -> entryDeps current binderDepth a
  PathCon _ -> Set.empty
  Flat a -> entryDeps current binderDepth a
  Sharp a -> entryDeps current binderDepth a
  Disc a -> entryDeps current binderDepth a
  Shape a -> entryDeps current binderDepth a
  Next a -> entryDeps current binderDepth a
  Eventually a -> entryDeps current binderDepth a
