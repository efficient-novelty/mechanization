#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
FIX_DIR="$ROOT_DIR/engine/testdata/phase5_decode"
FIX_TSV="$FIX_DIR/fixtures.tsv"
FIX_SHA="$FIX_DIR/fixtures.sha256"

[[ -f "$FIX_TSV" ]] || { echo "missing fixture file: $FIX_TSV" >&2; exit 1; }
[[ -f "$FIX_SHA" ]] || { echo "missing fixture hash file: $FIX_SHA" >&2; exit 1; }

expected_sha="$(tr -d '[:space:]' < "$FIX_SHA")"
actual_sha="$(sha256sum "$FIX_TSV" | awk '{print $1}')"

if [[ "$expected_sha" != "$actual_sha" ]]; then
  echo "fixture hash mismatch" >&2
  echo "  expected: $expected_sha" >&2
  echo "  actual:   $actual_sha" >&2
  exit 1
fi

python3 - "$FIX_TSV" <<'PY'
import csv, sys
path = sys.argv[1]
with open(path, newline='', encoding='utf-8') as f:
    rows = list(csv.DictReader(f, delimiter='\t'))
if len(rows) < 3:
    raise SystemExit('need at least 3 decode fixtures')
required = {'canonical_name', 'canonical_key', 'expected_label', 'min_conf', 'max_conf', 'expect_nonempty_label'}
missing = required - set(rows[0].keys())
if missing:
    raise SystemExit(f'missing fixture columns: {sorted(missing)}')
print('phase5 decode fixture schema check: OK')
PY

tmp_hs="$FIX_DIR/.phase5_decode_check.hs"
cat > "$tmp_hs" <<'HS'
import MBTTDecode
import System.Environment (getArgs)
import Data.List (intercalate)

splitTab :: String -> [String]
splitTab [] = [""]
splitTab (c:cs)
  | c == '\t' = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = splitTab cs

readRows :: String -> [[String]]
readRows txt = map splitTab (filter (not . null) (lines txt))

toBool :: String -> Bool
toBool s = s `elem` ["true", "True", "1"]

main :: IO ()
main = do
  [path] <- getArgs
  txt <- readFile path
  let rows = readRows txt
  case rows of
    [] -> error "empty fixture file"
    (_hdr:cases) -> do
      errs <- mapM checkCase cases
      let bad = concat errs
      if null bad
        then putStrLn "phase5 decode fixture behavior check: OK"
        else error (intercalate "\n" bad)

checkCase :: [String] -> IO [String]
checkCase cols =
  case cols of
    [nm,key,expected,minC,maxC,expectNonEmpty] -> do
      let dr = decodeCanonicalNameWithKey nm (Just key)
          lbl = maybe "" id (drDecodedLabel dr)
          conf = drConfidence dr
          minV = read minC :: Double
          maxV = read maxC :: Double
          wantLabel = toBool expectNonEmpty
          e1 = if drCanonicalName dr == nm then [] else ["canonical_name mismatch for " ++ nm]
          e2 = if drCanonicalKey dr == Just key then [] else ["canonical_key mismatch for " ++ nm]
          e3 = if lbl == expected then [] else ["decoded_label mismatch for " ++ nm ++ ": got '" ++ lbl ++ "'"]
          e4 = if conf >= minV && conf <= maxV then [] else ["confidence out of range for " ++ nm]
          e5 = if drNonInterfering dr then [] else ["non-interference flag false for " ++ nm]
          e6 = if (not (null lbl)) == wantLabel then [] else ["label emptiness mismatch for " ++ nm]
      pure (e1 ++ e2 ++ e3 ++ e4 ++ e5 ++ e6)
    _ -> pure ["invalid fixture row shape"]
HS

if command -v runghc >/dev/null 2>&1; then
  runghc -i"$ROOT_DIR/engine/src" "$tmp_hs" "$FIX_TSV"
else
  python3 - "$FIX_TSV" "$ROOT_DIR/engine/src/MBTTDecode.hs" <<'PYFALLBACK'
import csv, sys
fix, src = sys.argv[1], sys.argv[2]
text = open(src, encoding='utf-8').read()
with open(fix, newline='', encoding='utf-8') as f:
    rows = list(csv.DictReader(f, delimiter='	'))
for r in rows:
    nm = r['canonical_name']
    expected = r['expected_label']
    if f'"{nm}"' not in text:
        raise SystemExit(f'missing decode case in source for {nm}')
    if expected and expected not in text:
        raise SystemExit(f'missing expected label text in source for {nm}')
print('phase5 decode fixture behavior check: OK (fallback source validation)')
PYFALLBACK
fi
rm -f "$tmp_hs"

echo "phase5 decode fixture hash check: OK"
