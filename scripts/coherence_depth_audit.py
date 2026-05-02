#!/usr/bin/env python3
"""Audit coherence-depth case-study fixtures."""

from __future__ import annotations

import sys
from pathlib import Path


REQUIRED_KEYS = {
    "id",
    "title",
    "payload_fields",
    "active_interface_footprint",
    "unary_trace_obligations",
    "binary_trace_obligations",
    "higher_horn_obligations",
    "higher_horn_derived",
    "active_basis_totality",
    "expected_mu",
    "recurrence_law",
    "envelope",
    "agda_module",
    "doc",
    "presentation_group",
    "refactors",
}

INT_KEYS = {
    "payload_fields",
    "active_interface_footprint",
    "unary_trace_obligations",
    "binary_trace_obligations",
    "higher_horn_obligations",
    "expected_mu",
}

BOOL_KEYS = {"higher_horn_derived", "active_basis_totality", "refactors"}

RECURRENCE_LAWS = {
    "transparent-zero",
    "sparse-local",
    "promoted-active-basis",
    "full-coupling",
    "no-recurrence-law",
}


def parse_scalar(raw: str) -> object:
    text = raw.strip()
    lower = text.lower()
    if lower == "true":
        return True
    if lower == "false":
        return False
    if text.isdigit():
        return int(text)
    return text


def load_simple_yaml(path: Path) -> dict[str, object]:
    data: dict[str, object] = {}
    for line_no, line in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        stripped = line.strip()
        if not stripped or stripped.startswith("#"):
            continue
        if ":" not in stripped:
            raise ValueError(f"{path}:{line_no}: expected 'key: value'")
        key, value = stripped.split(":", 1)
        key = key.strip()
        if not key:
            raise ValueError(f"{path}:{line_no}: empty key")
        data[key] = parse_scalar(value)
    return data


def fail(errors: list[str], path: Path, message: str) -> None:
    errors.append(f"{path}: {message}")


def validate_fixture(path: Path, data: dict[str, object], errors: list[str]) -> None:
    missing = sorted(REQUIRED_KEYS.difference(data))
    if missing:
        fail(errors, path, f"missing required keys: {', '.join(missing)}")
        return

    for key in INT_KEYS:
        if not isinstance(data[key], int):
            fail(errors, path, f"{key} must be an integer")
        elif data[key] < 0:
            fail(errors, path, f"{key} must be non-negative")

    for key in BOOL_KEYS:
        if not isinstance(data[key], bool):
            fail(errors, path, f"{key} must be true or false")

    recurrence = data["recurrence_law"]
    if recurrence not in RECURRENCE_LAWS:
        fail(errors, path, f"unknown recurrence_law {recurrence!r}")
        return

    if data["higher_horn_obligations"] > 0 and not data["higher_horn_derived"]:
        fail(errors, path, "higher horn obligations must be marked derived")

    if recurrence == "transparent-zero":
        zero_keys = [
            "active_interface_footprint",
            "unary_trace_obligations",
            "binary_trace_obligations",
            "higher_horn_obligations",
            "expected_mu",
        ]
        for key in zero_keys:
            if data[key] != 0:
                fail(errors, path, f"transparent-zero requires {key} = 0")

    if recurrence == "sparse-local":
        if data["envelope"] == "full":
            fail(errors, path, "sparse-local must not claim the full envelope")
        if data["active_interface_footprint"] <= 0:
            fail(errors, path, "sparse-local requires a finite positive footprint")

    if recurrence == "full-coupling" and data["envelope"] != "full":
        fail(errors, path, "full-coupling requires envelope: full")

    if recurrence == "promoted-active-basis" and not data["active_basis_totality"]:
        fail(errors, path, "promoted-active-basis requires active_basis_totality")


def validate_refactors(fixtures: list[tuple[Path, dict[str, object]]], errors: list[str]) -> None:
    baselines: dict[str, int] = {}
    for path, data in fixtures:
        if data.get("refactors") is False:
            group = data.get("presentation_group")
            mu = data.get("expected_mu")
            if isinstance(group, str) and isinstance(mu, int):
                baselines.setdefault(group, mu)

    for path, data in fixtures:
        if data.get("refactors") is True:
            group = data.get("presentation_group")
            mu = data.get("expected_mu")
            if not isinstance(group, str) or group not in baselines:
                fail(errors, path, "refactored presentation has no baseline group")
            elif mu != baselines[group]:
                fail(errors, path, "refactored presentation changed expected_mu")


def main(argv: list[str]) -> int:
    if len(argv) != 2:
        print("usage: coherence_depth_audit.py RUNS_DIR", file=sys.stderr)
        return 2

    root = Path(argv[1])
    if not root.is_dir():
        print(f"not a directory: {root}", file=sys.stderr)
        return 2

    fixtures: list[tuple[Path, dict[str, object]]] = []
    errors: list[str] = []
    for path in sorted(root.glob("*.yaml")):
        try:
            data = load_simple_yaml(path)
        except ValueError as exc:
            errors.append(str(exc))
            continue
        validate_fixture(path, data, errors)
        fixtures.append((path, data))

    if not fixtures:
        errors.append(f"{root}: no yaml fixtures found")

    validate_refactors(fixtures, errors)

    if errors:
        for error in errors:
            print(f"ERROR: {error}", file=sys.stderr)
        return 1

    print(f"coherence-depth audit passed: {len(fixtures)} fixtures")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
