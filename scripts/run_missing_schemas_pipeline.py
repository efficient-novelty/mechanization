#!/usr/bin/env python3
from __future__ import annotations
from pathlib import Path
from datetime import datetime, timezone
import csv, json, subprocess, textwrap

BAR = 5.99
NU_BASE = 46
KAPPA_METRIC = 7

ROUTES = [
    {"route": "cauchy_minimal", "kappa_scalar": 4},
    {"route": "topological_arithmetic", "kappa_scalar": 3},
    {"route": "synthetic_continuum", "kappa_scalar": 2},
]

HS_PROGRAM = r'''
import MBTTNu
import Telescope
import TelescopeEval
import Types
import Kolmogorov (MBTTExpr(..))
import Data.List (isPrefixOf)

baseSteps :: [Int]
baseSteps = [1..12]

buildBase :: (Library, [(Int,Int)])
buildBase = go [] [] baseSteps
  where
    go lib nuHist [] = (lib, nuHist)
    go lib nuHist (s:ss) =
      let tele = referenceTelescope s
          nm = detectCanonicalName tele lib
          nr = computeNativeNu tele lib nuHist
          entry = telescopeToCandidate tele lib nm
      in go (lib ++ [entry]) (nuHist ++ [(s, nnTotal nr)]) ss

scalarTele :: String -> Telescope
scalarTele route = case route of
  "cauchy_minimal" -> Telescope
    [ TeleEntry "N-form" (App Univ (Var 1))
    , TeleEntry "Z-form" (Sigma (Var 1) (Var 1))
    , TeleEntry "Q-form" (Sigma (Var 1) (Pi (Var 1) (Var 1)))
    , TeleEntry "R-form" (Pi (Var 1) (Var 1))
    ]
  "topological_arithmetic" -> Telescope
    [ TeleEntry "Z-loop" (App (Lib 5) (Var 1))
    , TeleEntry "Q-quot" (Sigma (Var 1) (Var 1))
    , TeleEntry "R-comp" (Pi (Var 1) (Var 1))
    ]
  "synthetic_continuum" -> Telescope
    [ TeleEntry "R-smooth" (App (Lib 10) (Var 1))
    , TeleEntry "R-ring" (Sigma (Pi (Var 1) (Var 1)) (Pi (Var 1) (Var 1)))
    ]
  _ -> Telescope []

kappaScalar :: String -> Int
kappaScalar route = case route of
  "cauchy_minimal" -> 4
  "topological_arithmetic" -> 3
  "synthetic_continuum" -> 2
  _ -> 0

nodeLines :: NativeNuResult -> [String]
nodeLines nr = filter (isPrefixOf "node=") (nnTrace nr)

emitRoute :: String -> IO ()
emitRoute route = do
  let (libBase, nuHist) = buildBase
      tScalar = scalarTele route
      tMetric = referenceTelescope 13
      tCombined = Telescope (teleEntries tScalar ++ teleEntries tMetric)
      nScalar = computeNativeNu tScalar libBase nuHist
      nMetric = computeNativeNu tMetric libBase nuHist
      nCombined = computeNativeNu tCombined libBase nuHist
      kS = kappaScalar route
      kF = 7 + kS
      dMin = ceiling (5.99 * fromIntegral kF - 46.0) :: Int
      dMech = nnTotal nCombined - nnTotal nMetric - nnTotal nScalar
  putStrLn $ "ROUTE|" ++ route ++ "|" ++ show kS ++ "|" ++ show kF ++ "|"
          ++ show (nnTotal nScalar) ++ "|" ++ show (nnTotal nMetric) ++ "|"
          ++ show (nnTotal nCombined) ++ "|" ++ show dMin ++ "|" ++ show dMech
  mapM_ (\ln -> putStrLn $ "TRACE|" ++ route ++ "|scalar|" ++ ln) (nodeLines nScalar)
  mapM_ (\ln -> putStrLn $ "TRACE|" ++ route ++ "|metric|" ++ ln) (nodeLines nMetric)
  mapM_ (\ln -> putStrLn $ "TRACE|" ++ route ++ "|combined|" ++ ln) (nodeLines nCombined)

main :: IO ()
main = do
  mapM_ emitRoute ["cauchy_minimal","topological_arithmetic","synthetic_continuum"]
'''


def parse_trace_sig(node_line: str) -> str:
    # node=entry1/0|ctor=Pi -> Pi@entry1/0
    parts = node_line.split("|")
    node = parts[0].split("=",1)[1]
    ctor = parts[1].split("=",1)[1] if len(parts) > 1 else "Unknown"
    return f"{ctor}@{node}"


def main():
    run_id = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    root = Path("runs") / "missing_schemas" / run_id
    root.mkdir(parents=True, exist_ok=True)

    hs_path = root / "_missing_schemas_native.hs"
    hs_path.write_text(textwrap.dedent(HS_PROGRAM))

    cmd = ["runghc", "-iengine/src", str(hs_path)]
    proc = subprocess.run(cmd, capture_output=True, text=True, check=True)
    (root / "native_stdout.log").write_text(proc.stdout)
    (root / "native_stderr.log").write_text(proc.stderr)

    route_rows = []
    traces = []
    for ln in proc.stdout.splitlines():
        if ln.startswith("ROUTE|"):
            _, route, ks, kf, nus, num, nuc, dmin, dmech = ln.split("|")
            route_rows.append({
                "route": route,
                "kappa_scalar": int(ks),
                "kappa_full": int(kf),
                "nu_scalar": int(nus),
                "nu_metric": int(num),
                "nu_combined": int(nuc),
                "delta_nu_min": int(dmin),
                "delta_nu_mech": int(dmech),
            })
        elif ln.startswith("TRACE|"):
            _, route, trace_set, node_line = ln.split("|", 3)
            traces.append({"route": route, "trace_set": trace_set, "node_line": node_line, "schema_sig": parse_trace_sig(node_line)})

    # sets by route
    by_route = {}
    for r in route_rows:
        by_route[r["route"]] = {"scalar": set(), "metric": set(), "combined": set()}
    for t in traces:
        by_route[t["route"]][t["trace_set"]].add(t["schema_sig"])

    targets = []
    raw_rows = []
    canon_rows = []
    verdicts = []

    for r in route_rows:
        route = r["route"]
        ks = r["kappa_scalar"]
        kf = r["kappa_full"]
        dmin = r["delta_nu_min"]
        dmech = r["delta_nu_mech"]

        targets.append({
            "route": route,
            "kappa_scalar": ks,
            "kappa_full": kf,
            "nu_base": NU_BASE,
            "bar": BAR,
            "delta_nu_min": dmin,
        })

        scalar_set = by_route[route]["scalar"]
        metric_set = by_route[route]["metric"]
        combined_set = by_route[route]["combined"]

        for i, sig in enumerate(sorted(combined_set), start=1):
            in_scalar = sig in scalar_set
            in_metric = sig in metric_set
            is_new = (not in_scalar) and (not in_metric)
            row = {
                "route": route,
                "schema_id": f"{route}:{i:04d}",
                "family": "engine_native_node_trace",
                "type_expr": sig,
                "witness": f"nnTrace:{sig}",
                "novelty_bucket": "native_nu_trace",
                "in_L_metric": str(in_metric).lower(),
                "in_L_scalar": str(in_scalar).lower(),
                "in_L_combined": "true",
                "is_interaction_new": str(is_new).lower(),
                "canonical_key": sig.lower(),
            }
            raw_rows.append(row)

        # canonical dedupe by canonical_key
        seen = set()
        for rr in [x for x in raw_rows if x["route"] == route and x["is_interaction_new"] == "true"]:
            ck = rr["canonical_key"]
            if ck in seen:
                continue
            seen.add(ck)
            canon_rows.append(rr)

        ccount = len([x for x in canon_rows if x["route"] == route])
        nu_new = NU_BASE + dmech
        rho_new = nu_new / kf
        verdicts.append({
            "route": route,
            "kappa_scalar": ks,
            "kappa_full": kf,
            "nu_base": NU_BASE,
            "delta_nu_min": dmin,
            "delta_nu_mech": dmech,
            "nu_new": nu_new,
            "rho_new": round(rho_new, 6),
            "bar": BAR,
            "margin": round(rho_new - BAR, 6),
            "pass": str(dmech >= dmin).lower(),
            "raw_new_count": len([x for x in raw_rows if x["route"] == route and x["is_interaction_new"] == "true"]),
            "canonical_new_count": ccount,
            "dedupe_ratio": round(ccount / max(1, len([x for x in raw_rows if x["route"] == route and x["is_interaction_new"] == "true"])), 6),
            "nu_scalar": r["nu_scalar"],
            "nu_metric": r["nu_metric"],
            "nu_combined": r["nu_combined"],
        })

    # write artifacts
    manifest = {
        "run_id": run_id,
        "timestamp_utc": run_id,
        "bar": BAR,
        "nu_base": NU_BASE,
        "kappa_metric": KAPPA_METRIC,
        "policy": "strict_v1_first_use_full_charge",
        "routes": ROUTES,
        "generator": "scripts/run_missing_schemas_pipeline.py",
        "trace_source": "engine/src/MBTTNu.computeNativeNu nnTrace node records",
        "engine_execution": "runghc -iengine/src",
    }
    (root / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")

    def write_csv(path: Path, rows):
        if not rows:
            path.write_text("")
            return
        with path.open("w", newline="") as f:
            w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
            w.writeheader(); w.writerows(rows)

    write_csv(root / "targets.csv", targets)
    write_csv(root / "schemas_raw.csv", raw_rows)
    write_csv(root / "schemas_canonical.csv", canon_rows)
    write_csv(root / "verdicts.csv", verdicts)

    controls = []
    for route in by_route:
        controls.append({"route": route, "control": "metric_only", "count": len(by_route[route]["metric"])})
        controls.append({"route": route, "control": "scalar_only", "count": len(by_route[route]["scalar"])})
        controls.append({"route": route, "control": "combined", "count": len(by_route[route]["combined"])})
    write_csv(root / "controls.csv", controls)

    dedupe_lines = ["# Dedupe Report", "", f"raw_rows_total={len(raw_rows)}", f"canonical_rows_total={len(canon_rows)}"]
    (root / "dedupe_report.md").write_text("\n".join(dedupe_lines) + "\n")

    best = sorted(verdicts, key=lambda x: x["margin"], reverse=True)[0]
    lines = [
        "# Missing Schemas Mechanical Run Summary",
        "",
        f"run_id: {run_id}",
        f"policy: strict V1, bar={BAR}",
        "",
        "## Route verdicts",
    ]
    for v in verdicts:
        lines.append(
            f"- {v['route']}: kappa_full={v['kappa_full']}, delta_nu_min={v['delta_nu_min']}, "
            f"delta_nu_mech={v['delta_nu_mech']}, nu_scalar={v['nu_scalar']}, nu_metric={v['nu_metric']}, "
            f"nu_combined={v['nu_combined']}, rho_new={v['rho_new']}, margin={v['margin']}, pass={v['pass']}"
        )
    lines += [
        "",
        f"Best route by margin: {best['route']} (margin={best['margin']}).",
        "",
        "## Discussion",
        "- Witnesses are now engine-native MBTT node traces from computeNativeNu (nnTrace node=...|ctor=...).",
        "- delta_nu_mech uses inclusion-exclusion over native nu totals: nu_combined - nu_metric - nu_scalar.",
        "- Next refinement: tie node traces to inhabited-type deltas in uniform-nu replay lane.",
    ]
    (root / "summary.md").write_text("\n".join(lines) + "\n")

    print(str(root))


if __name__ == "__main__":
    main()
