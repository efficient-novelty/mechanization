{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Parallel computation utilities for PEN engine
--
-- Provides parallelism infrastructure for type enumeration,
-- candidate evaluation, and parameter sweeps using 8 CPU cores.

module Parallel
  ( parMapChunked
  , parMapList
  , parEnumeratePartitioned
  , numCPUs
  ) where

import Control.Parallel.Strategies
import Control.DeepSeq ()

-- | Number of available CPUs (configured at compile time via -N8)
numCPUs :: Int
numCPUs = 8

-- | Parallel map with chunking for balanced load distribution.
-- Splits the input list into chunks (one per CPU) and evaluates
-- each chunk in parallel using 'parList rdeepseq'.
parMapChunked :: NFData b => (a -> b) -> [a] -> [b]
parMapChunked f xs =
  let chunks = splitIntoChunks numCPUs xs
      results = map (map f) chunks `using` parList rdeepseq
  in concat results

-- | Parallel map over a list using parList strategy.
-- Each element is evaluated in parallel to full normal form.
-- Use for lists where each element is roughly equal work.
parMapList :: NFData b => (a -> b) -> [a] -> [b]
parMapList f xs = map f xs `using` parList rdeepseq

-- | Partition a list of atoms into groups, enumerate each group
-- in parallel, and merge results.
-- The enumeration function takes a sublist of atoms and returns results.
parEnumeratePartitioned :: NFData b => Int -> [a] -> ([a] -> [b]) -> [b]
parEnumeratePartitioned nChunks atoms enumerate =
  let chunks = splitIntoChunks nChunks atoms
      results = map enumerate chunks `using` parList rdeepseq
  in concat results

-- | Split a list into n roughly equal chunks.
splitIntoChunks :: Int -> [a] -> [[a]]
splitIntoChunks _ [] = []
splitIntoChunks n xs
  | n <= 0    = [xs]
  | otherwise = go xs
  where
    len = length xs
    chunkSize = max 1 ((len + n - 1) `div` n)
    go [] = []
    go ys = let (chunk, rest) = splitAt chunkSize ys
            in chunk : go rest
