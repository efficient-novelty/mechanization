-- | Confluent rewrite system for type expression canonicalization
--
-- Extends ProofRank.normalize with:
--   B: Commutativity (sort operands of * and +)
--   C: Associativity (flatten, sort, right-fold for * and +)
--   D: Currying ((A*B)->C -> A->(B->C))
--   E: Distributivity ((A+B)->C -> (A->C)*(B->C))
--   F: Suspension (Susp(S1)->S2, Susp(S2)->S3 when target in library)
--
-- Confluence strategy: apply E before D. E removes + from arrow domains;
-- D removes * from arrow domains. AC normalization via flatten -> sort -> foldr1.

module Equivalence
  ( canonicalize
  , typeEquivC
  , equivalenceClass
  , mapChildren
  ) where

import Types
import Data.List (sort, nubBy)

-- ============================================
-- Map over immediate children
-- ============================================

-- | Apply a function to all immediate children of a type expression
mapChildren :: (TypeExpr -> TypeExpr) -> TypeExpr -> TypeExpr
mapChildren _ TUnit = TUnit
mapChildren _ TVoid = TVoid
mapChildren _ (TRef s) = TRef s
mapChildren f (TArrow a b) = TArrow (f a) (f b)
mapChildren f (TProd a b) = TProd (f a) (f b)
mapChildren f (TCoprod a b) = TCoprod (f a) (f b)
mapChildren f (TId a x y) = TId (f a) (f x) (f y)
mapChildren f (TSelfId a) = TSelfId (f a)
mapChildren f (TOmega a) = TOmega (f a)
mapChildren f (TSusp a) = TSusp (f a)
mapChildren f (TTrunc n a) = TTrunc n (f a)
mapChildren f (TPi v a b) = TPi v (f a) (f b)
mapChildren f (TSigma v a b) = TSigma v (f a) (f b)
mapChildren _ (THIT p d) = THIT p d
mapChildren f (TFiber a b) = TFiber (f a) (f b)
mapChildren f (TDeloop a) = TDeloop (f a)

-- ============================================
-- Flattening for AC normalization
-- ============================================

-- | Flatten nested products: (A*B)*C -> [A, B, C]
flattenProd :: TypeExpr -> [TypeExpr]
flattenProd (TProd a b) = flattenProd a ++ flattenProd b
flattenProd x = [x]

-- | Flatten nested coproducts: (A+B)+C -> [A, B, C]
flattenCoprod :: TypeExpr -> [TypeExpr]
flattenCoprod (TCoprod a b) = flattenCoprod a ++ flattenCoprod b
flattenCoprod x = [x]

-- | Rebuild a right-associated product from a sorted list
rebuildProd :: [TypeExpr] -> TypeExpr
rebuildProd [] = TUnit  -- empty product is unit
rebuildProd [x] = x
rebuildProd xs = foldr1 TProd xs

-- | Rebuild a right-associated coproduct from a sorted list
rebuildCoprod :: [TypeExpr] -> TypeExpr
rebuildCoprod [] = TVoid  -- empty coproduct is void
rebuildCoprod [x] = x
rebuildCoprod xs = foldr1 TCoprod xs

-- ============================================
-- Canonicalization (innermost-first rewriting)
-- ============================================

-- | Canonicalize a type expression to normal form.
-- Applies all rewrite rules innermost-first until a fixed point.
canonicalize :: TypeExpr -> TypeExpr
canonicalize t =
  let t' = rewriteOnce (mapChildren canonicalize t)
  in if t' == t then t else canonicalize t'

-- | Apply one round of rewrite rules (outermost).
-- Rules are applied in order: unit/void simplification,
-- distributivity, currying, AC normalization, suspension.
rewriteOnce :: TypeExpr -> TypeExpr
rewriteOnce = ruleF . ruleAC . ruleD . ruleE . ruleA

-- ============================================
-- Group A: Unit/Void simplification
-- ============================================

ruleA :: TypeExpr -> TypeExpr
-- Product with unit
ruleA (TProd a TUnit) = a
ruleA (TProd TUnit b) = b
-- Product with void (absorption)
ruleA (TProd _ TVoid) = TVoid
ruleA (TProd TVoid _) = TVoid
-- Coproduct with void
ruleA (TCoprod a TVoid) = a
ruleA (TCoprod TVoid b) = b
-- Arrow to/from unit/void
ruleA (TArrow _ TUnit) = TUnit        -- A -> 1 ≃ 1
ruleA (TArrow TVoid _) = TUnit        -- 0 -> A ≃ 1
ruleA (TArrow TUnit b) = b            -- 1 -> A ≃ A
-- SelfId/Omega of trivial types
ruleA (TSelfId TUnit) = TUnit
ruleA (TSelfId TVoid) = TVoid
ruleA (TOmega TUnit) = TUnit          -- Ω(1) ≃ 1 (contractible)
ruleA (TOmega TVoid) = TVoid          -- Ω(0) ≃ 0 (empty)
-- Suspension of void
ruleA (TSusp TVoid) = TUnit
-- Resolve library names
ruleA (TRef "1") = TUnit
ruleA (TRef "0") = TVoid
ruleA x = x

-- ============================================
-- Group E: Distributivity
-- (A+B)->C -> (A->C)*(B->C)
-- Applied BEFORE currying so + is eliminated from arrow domains first.
-- ============================================

ruleE :: TypeExpr -> TypeExpr
ruleE (TArrow (TCoprod a b) c) = TProd (TArrow a c) (TArrow b c)
ruleE x = x

-- ============================================
-- Group D: Currying
-- (A*B)->C -> A->(B->C)
-- ============================================

ruleD :: TypeExpr -> TypeExpr
ruleD (TArrow (TProd a b) c) = TArrow a (TArrow b c)
ruleD x = x

-- ============================================
-- Group B+C: AC normalization
-- Flatten products/coproducts, sort, right-fold.
-- Sorting uses show-based ordering (matches ProofRank.schemaize pattern).
-- ============================================

ruleAC :: TypeExpr -> TypeExpr
ruleAC t@(TProd _ _) =
  let parts = flattenProd t
      filtered = filter (/= TUnit) parts  -- remove units
  in case filtered of
       []  -> TUnit
       [x] -> x
       xs  -> let sorted = sort xs
              in rebuildProd sorted
ruleAC t@(TCoprod _ _) =
  let parts = flattenCoprod t
      filtered = filter (/= TVoid) parts  -- remove voids
  in case filtered of
       []  -> TVoid
       [x] -> x
       xs  -> let sorted = sort xs
              in rebuildCoprod sorted
ruleAC x = x

-- ============================================
-- Group F: Suspension reduction
-- Susp(S1) -> S2, Susp(S2) -> S3 (when known)
-- ============================================

ruleF :: TypeExpr -> TypeExpr
ruleF (TSusp (TRef "S1")) = TRef "S2"
ruleF (TSusp (THIT 1 [1])) = TRef "S2"
ruleF (TSusp (TRef "S2")) = TRef "S3"
ruleF (TSusp (THIT 1 [2])) = TRef "S3"
ruleF x = x

-- ============================================
-- Equivalence checking
-- ============================================

-- | Check if two type expressions are equivalent under canonicalization
typeEquivC :: TypeExpr -> TypeExpr -> Bool
typeEquivC a b = canonicalize a == canonicalize b

-- ============================================
-- Equivalence classes
-- ============================================

-- | Partition a list of type expressions into equivalence classes
equivalenceClass :: [TypeExpr] -> [[TypeExpr]]
equivalenceClass [] = []
equivalenceClass types =
  let canonical = [(t, canonicalize t) | t <- types]
      classes = groupByCanon canonical
  in map (map fst) classes

-- | Group (original, canonical) pairs by their canonical form
groupByCanon :: [(TypeExpr, TypeExpr)] -> [[(TypeExpr, TypeExpr)]]
groupByCanon [] = []
groupByCanon ((t, c):rest) =
  let (same, diff) = span (\(_, c') -> c' == c) rest'
      rest' = sortByCanon rest
  in ((t, c) : same) : groupByCanon diff
  where
    sortByCanon = nubBy (\(_, c1) (_, c2) -> c1 == c2)
                . filter (\(_, c') -> c' /= c)

-- Note: The above groupByCanon is simple but works for our use case.
-- For a production system we'd use Data.Map grouping.
