-- | Coherence Window parameterization (Prong A1)
--
-- Generalizes the hardcoded d=2 (Fibonacci) coherence window to arbitrary
-- d-bonacci sequences.  The d-bonacci sequence of order d is defined by:
--
--   delta_1 = ... = delta_d = 1
--   delta_n = delta_{n-1} + delta_{n-2} + ... + delta_{n-d}   for n > d
--
-- Special cases:
--   d=1: [1, 1, 1, 1, ...]           (constant — trivial window)
--   d=2: [1, 1, 2, 3, 5, 8, ...]     (Fibonacci — the PEN default)
--   d=3: [1, 1, 2, 4, 7, 13, 24, ...]  (tribonacci)
--
-- The window depth d controls how many prior library entries are visible
-- when evaluating a new candidate's proof-rank.

module CoherenceWindow
  ( dBonacci
  , dBonacciDelta
  , dBonacciTau
  , defaultWindow
  ) where

-- | Generate the infinite d-bonacci sequence.
--
-- dBonacci 1 = [1, 1, 1, 1, ...]         (constant)
-- dBonacci 2 = [1, 1, 2, 3, 5, 8, ...]   (Fibonacci)
-- dBonacci 3 = [1, 1, 2, 4, 7, 13, ...]  (tribonacci)
dBonacci :: Int -> [Int]
dBonacci d
  | d <= 0    = repeat 1
  | d == 1    = repeat 1
  | otherwise = let go prev = let next = sum (take d prev)
                              in next : go (next : prev)
                    initial = replicate d 1  -- d initial 1s
                in reverse initial ++ go initial

-- | Get delta_n for the d-bonacci sequence (1-indexed).
--
-- dBonacciDelta 2 1 = 1   (F_1)
-- dBonacciDelta 2 2 = 1   (F_2)
-- dBonacciDelta 2 3 = 2   (F_3)
-- dBonacciDelta 2 5 = 5   (F_5)
dBonacciDelta :: Int -> Int -> Int
dBonacciDelta d n
  | n < 1     = 1
  | otherwise = dBonacci d !! (n - 1)

-- | Cumulative tau_n = sum of first n deltas.
--
-- dBonacciTau 2 3 = 1 + 1 + 2 = 4   (tau_3 for Fibonacci)
dBonacciTau :: Int -> Int -> Int
dBonacciTau d n = sum (take n (dBonacci d))

-- | Default coherence window depth (Fibonacci).
defaultWindow :: Int
defaultWindow = 2
