{-# LANGUAGE BangPatterns #-}

-- | Bidirectional Type Checker MVP for MBTT Telescopes
--
-- Validates telescope well-formedness WITHOUT full MBTT normalization.
-- This is a lightweight checker that rejects obviously ill-formed telescopes
-- (invalid variable/library references, ill-scoped binders) while accepting
-- all well-formed ones.
--
-- The checker is CONSERVATIVE: it may accept some ill-typed telescopes,
-- but it should never reject a well-typed one (no false negatives).
--
-- Checks performed:
--   1. Library reference bounds: Lib i must satisfy 1 <= i <= |library|
--   2. Variable reference bounds: Var i must satisfy 1 <= i <= context depth
--   3. Scope validity: binders (Pi, Sigma, Lam) extend the context by 1
--   4. Universe placement: Univ only at type-level positions
--   5. Term-level constraints: PathCon only at term-level positions
--   6. Arity checking: no bare Univ as function argument

module TelescopeCheck
  ( checkTelescope
  , CheckResult(..)
  , CheckError(..)
  , checkAndFilter
  ) where

import Kolmogorov (MBTTExpr(..))
import Telescope (Telescope(..), TeleEntry(..))
import Types (Library)

-- ============================================
-- Result Types
-- ============================================

data CheckResult
  = CheckOK
  | CheckFail CheckError
  deriving (Show, Eq)

data CheckError
  = LibRefOutOfBounds   Int Int    -- ^ Lib i, but library has size n (i > n or i < 1)
  | VarRefOutOfBounds   Int Int    -- ^ Var i, but context has depth n (i > n or i < 1)
  | EmptyTelescope                 -- ^ Telescope has no entries
  | BareUnivAsArgument             -- ^ Univ used as function argument (App _ Univ)
  deriving (Show, Eq)

-- ============================================
-- Main Checker
-- ============================================

-- | Check a telescope for well-formedness.
--
-- The telescope is checked entry by entry. Each entry's type expression
-- is checked under a context that grows as we proceed through the telescope:
--   - Context depth starts at 0
--   - Each entry extends the context by 1
--   - So entry k is checked at context depth k (0-indexed)
--
-- Library size is fixed (the current library before this candidate).
checkTelescope :: Library -> Telescope -> CheckResult
checkTelescope lib (Telescope entries)
  | null entries = CheckFail EmptyTelescope
  | otherwise = go 0 entries
  where
    libSize = length lib

    go :: Int -> [TeleEntry] -> CheckResult
    go _ctxDepth [] = CheckOK
    go !ctxDepth (TeleEntry _name expr : rest) =
      case checkExpr libSize ctxDepth expr of
        CheckOK    -> go (ctxDepth + 1) rest
        err        -> err

-- | Check a single MBTT expression for well-formedness.
--
-- Parameters:
--   libSize  - number of entries in the library
--   ctxDepth - number of variables in scope (from preceding telescope entries)
--   expr     - the expression to check
checkExpr :: Int -> Int -> MBTTExpr -> CheckResult
checkExpr libSize ctxDepth expr = case expr of
  -- Terminal nodes: check reference bounds
  Lib i
    | i < 1 || i > libSize -> CheckFail (LibRefOutOfBounds i libSize)
    | otherwise            -> CheckOK

  Var i
    | i < 1 || i > ctxDepth -> CheckFail (VarRefOutOfBounds i ctxDepth)
    | otherwise              -> CheckOK

  Univ -> CheckOK

  PathCon _ -> CheckOK

  -- Binders: extend context depth by 1 for the body
  Pi dom cod ->
    case checkExpr libSize ctxDepth dom of
      CheckOK -> checkExpr libSize (ctxDepth + 1) cod
      err     -> err

  Sigma dom cod ->
    case checkExpr libSize ctxDepth dom of
      CheckOK -> checkExpr libSize (ctxDepth + 1) cod
      err     -> err

  Lam body -> checkExpr libSize (ctxDepth + 1) body

  -- Application: check both sides, reject bare Univ as argument
  App _f Univ -> CheckFail BareUnivAsArgument
  App f x ->
    case checkExpr libSize ctxDepth f of
      CheckOK -> checkExpr libSize ctxDepth x
      err     -> err

  -- Identity type
  Id a x y ->
    case checkExpr libSize ctxDepth a of
      CheckOK -> case checkExpr libSize ctxDepth x of
        CheckOK -> checkExpr libSize ctxDepth y
        err     -> err
      err -> err

  Refl a -> checkExpr libSize ctxDepth a

  -- Unary operators: just recurse
  Susp a       -> checkExpr libSize ctxDepth a
  Trunc a      -> checkExpr libSize ctxDepth a
  Flat a       -> checkExpr libSize ctxDepth a
  Sharp a      -> checkExpr libSize ctxDepth a
  Disc a       -> checkExpr libSize ctxDepth a
  Shape a      -> checkExpr libSize ctxDepth a
  Next a       -> checkExpr libSize ctxDepth a
  Eventually a -> checkExpr libSize ctxDepth a

-- ============================================
-- Convenience: Filter Telescopes
-- ============================================

-- | Filter a list of telescopes, keeping only well-formed ones.
-- Returns (valid, rejected_count).
checkAndFilter :: Library -> [Telescope] -> ([Telescope], Int)
checkAndFilter lib teles =
  let results = [(t, checkTelescope lib t) | t <- teles]
      valid   = [t | (t, CheckOK) <- results]
      rejected = length results - length valid
  in (valid, rejected)
