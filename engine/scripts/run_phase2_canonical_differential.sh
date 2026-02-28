#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
ENGINE_DIR="$ROOT_DIR/engine"
STAMP="$(date -u +%Y%m%dT%H%M%SZ)"
OUT_DIR_INPUT="${1:-$ROOT_DIR/runs/phase2_differential/$STAMP}"
if [[ "$OUT_DIR_INPUT" = /* ]]; then
  OUT_DIR="$OUT_DIR_INPUT"
else
  OUT_DIR="$ROOT_DIR/$OUT_DIR_INPUT"
fi

LIB_STEP="${LIB_STEP:-3}"
MAX_BIT_BUDGET="${MAX_BIT_BUDGET:-16}"
MAX_ENTRIES="${MAX_ENTRIES:-2}"
MAX_AST_DEPTH="${MAX_AST_DEPTH:-2}"
MAX_CANDIDATES="${MAX_CANDIDATES:-120}"
THRESHOLD_PCT="${THRESHOLD_PCT:-40}"

mkdir -p "$OUT_DIR"

pushd "$ENGINE_DIR" >/dev/null

tmp_hs="$OUT_DIR/p2_v4_differential.hs"
cat > "$tmp_hs" <<'HS'
import MBTTEnum
import MBTTCanonical
import Telescope
import Types
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)
import Text.Printf (printf)

buildLib :: Int -> Library
buildLib n = go [] 1
  where
    names = ["Universe","Unit","Witness","Pi","S1","Trunc","S2","S3","Hopf","Cohesion","Connections","Curvature","Metric","Hilbert","DCT"]
    go lib i
      | i > n = lib
      | otherwise =
          let tele = referenceTelescope i
              name = names !! (i - 1)
          in go (lib ++ [teleToEntry tele name]) (i + 1)

dedupByKey :: [Telescope] -> [Telescope]
dedupByKey teles = reverse (snd (foldl step (Map.empty, []) teles))
  where
    step (seen, acc) t =
      let k = canonicalKeySpec (map teType (teleEntries t))
      in case Map.lookup k seen of
           Just _  -> (seen, acc)
           Nothing -> (Map.insert k () seen, t : acc)

main :: IO ()
main = do
  [outDir, libStepS, bitS, entriesS, depthS, candS, thresholdS] <- getArgs
  let libStep = read libStepS :: Int
      maxBit = read bitS :: Int
      maxEntries = read entriesS :: Int
      maxDepth = read depthS :: Int
      maxCands = read candS :: Int
      threshold = read thresholdS :: Double
      lib = buildLib libStep
      cfg = defaultEnumConfig
        { ecMaxBitBudget = maxBit
        , ecMaxEntries = maxEntries
        , ecMaxASTDepth = maxDepth
        , ecMaxCandidates = maxCands
        }
      raw = map mcTelescope (enumerateMBTTTelescopes lib cfg)
      canon = dedupByKey raw
      rawN = length raw
      canonN = length canon
      reduction = if rawN > 0 then (1.0 - fromIntegral canonN / fromIntegral rawN) * 100.0 else 0.0
      meets = reduction >= threshold
      keySample = take 5 [ unCanonKey (canonicalKeySpec (map teType (teleEntries t))) | t <- canon ]

  writeFile (outDir ++ "/frontier_counts.csv") $ unlines
    ["mode,raw_candidates,canonical_candidates,reduction_pct"
    ,printf "canonical_on,%d,%d,%.2f" rawN canonN reduction
    ,printf "canonical_off,%d,%d,%.2f" rawN rawN (0.0 :: Double)
    ]

  writeFile (outDir ++ "/report.md") $ unlines
    (["# Phase 2 V4 Differential Report"
    ,""
    ,printf "- lib_step: %d" libStep
    ,printf "- max_bit_budget: %d" maxBit
    ,printf "- max_entries: %d" maxEntries
    ,printf "- max_ast_depth: %d" maxDepth
    ,printf "- max_candidates: %d" maxCands
    ,printf "- raw_candidates (OFF baseline): %d" rawN
    ,printf "- canonical_candidates (ON): %d" canonN
    ,printf "- reduction_pct: %.2f%%" reduction
    ,printf "- target_pct: %.2f%%" threshold
    ,printf "- meets_target: %s" (if meets then "true" else "false")
    ,""
    ,"## Canonical key sample"] ++ map ("- " ++) keySample)

  if meets then return () else error (printf "P2-V4 threshold not met: %.2f%% < %.2f%%" reduction threshold)
HS

runghc -i./src "$tmp_hs" "$OUT_DIR" "$LIB_STEP" "$MAX_BIT_BUDGET" "$MAX_ENTRIES" "$MAX_AST_DEPTH" "$MAX_CANDIDATES" "$THRESHOLD_PCT"
rm -f "$tmp_hs"

popd >/dev/null


echo "Wrote P2-V4 differential artifacts to: $OUT_DIR"
