#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
ENGINE_DIR="$ROOT_DIR/engine"
STAMP="$(date -u +%Y%m%dT%H%M%SZ)"
OUT_DIR_INPUT="${1:-$ROOT_DIR/runs/phase3_native_nu/$STAMP}"
if [[ "$OUT_DIR_INPUT" = /* ]]; then
  OUT_DIR="$OUT_DIR_INPUT"
else
  OUT_DIR="$ROOT_DIR/$OUT_DIR_INPUT"
fi

STEPS="${STEPS:-1 2 3 4 5 6}"
mkdir -p "$OUT_DIR"

pushd "$ENGINE_DIR" >/dev/null

tmp_hs="$OUT_DIR/p3_v5_native_nu_evidence.hs"
cat > "$tmp_hs" <<'HS'
import MBTTNu
import Telescope
import TelescopeEval
import Types
import Data.List (isPrefixOf, intercalate)
import Text.Printf (printf)
import System.Environment (getArgs)

requiredPrefixes :: [String]
requiredPrefixes =
  [ "source="
  , "nu_g="
  , "nu_h="
  , "nu_c="
  , "bonus_distributive="
  , "bonus_universe_poly="
  , "bonus_infinitesimal_shift="
  , "nu_total="
  , "node_trace_count="
  ]

hasRequired :: [String] -> Bool
hasRequired tr = all (\pre -> any (isPrefixOf pre) tr) requiredPrefixes

main :: IO ()
main = do
  [outDir, stepsArg] <- getArgs
  let steps = map read (words stepsArg) :: [Int]
  rows <- go [] [] steps

  let header = "step,name,nu_total,node_trace_count,trace_line_count,required_keys"
      csvRows = [ intercalate ","
                  [ show st, nm, show nu, show nc, show tl, if ok then "true" else "false" ]
                | (st,nm,nu,nc,tl,ok) <- rows]
  writeFile (outDir ++ "/native_nu_trace_summary.csv") (unlines (header:csvRows))

  let totalSteps = length rows
      allKeys = and [ok | (_,_,_,_,_,ok) <- rows]
      minNodes = minimum [nc | (_,_,_,nc,_,_) <- rows]
      ok = totalSteps > 0 && allKeys && minNodes > 0

  writeFile (outDir ++ "/report.md") $ unlines
    [ "# Phase 3 Native Nu Evidence Report"
    , ""
    , "- steps: " ++ show steps
    , "- rows: " ++ show totalSteps
    , "- all_required_trace_keys: " ++ toLowerBool allKeys
    , "- min_node_trace_count: " ++ show minNodes
    , "- status: " ++ (if ok then "pass" else "fail")
    ]

  if ok then return () else error "phase3 native-nu evidence checks failed"

  where
    toLowerBool True = "true"
    toLowerBool False = "false"

    go :: Library -> [(Int,Int)] -> [Int] -> IO [(Int,String,Int,Int,Int,Bool)]
    go _ _ [] = return []
    go lib nuHist (step:rest) = do
      let tele = referenceTelescope step
          name = detectCanonicalName tele lib
          nr = computeNativeNu tele lib nuHist
          tr = nnTrace nr
          nodeCount = length [x | x <- tr, "node=" `isPrefixOf` x]
          keysOk = hasRequired tr
          this = (step, name, nnTotal nr, nodeCount, length tr, keysOk)
          entry = telescopeToCandidate tele lib name
      rs <- go (lib ++ [entry]) (nuHist ++ [(step, nnTotal nr)]) rest
      return (this:rs)
HS

runghc -i./src "$tmp_hs" "$OUT_DIR" "$STEPS"
rm -f "$tmp_hs"

popd >/dev/null

echo "Wrote P3 native-nu evidence artifacts to: $OUT_DIR"
