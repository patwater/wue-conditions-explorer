#!/bin/bash
# Runs the generator and the estimator twice with the same seed on different core counts and compares file hashes.
# Writes reports/reproducibility.csv (one row per file) for 10_validate.R.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
PROFILE="${1:-default}"
for C in 1 4; do
  OUT="$ROOT/build/repro/$PROFILE/cores$C"
  rm -rf "$OUT"
  for S in 01_geography 02_accounts 03_occupancy 04_climate 05_usage 06_treatment 07_billing 08_observation 09_estimate_savings; do
    Rscript "$ROOT/R/$S.R" --profile "$PROFILE" --cores "$C" --out "$OUT" --force > /dev/null 2>&1
  done
done
A="$ROOT/build/repro/$PROFILE/cores1"; B="$ROOT/build/repro/$PROFILE/cores4"
{
  echo "file,sha256_cores1,sha256_cores4,identical"
  (cd "$A" && find data truth estimates -type f | sort) | while read -r f; do
    h1=$(sha256sum "$A/$f" | cut -d' ' -f1); h2=$(sha256sum "$B/$f" 2>/dev/null | cut -d' ' -f1 || echo missing)
    if [ "$h1" = "$h2" ]; then same=TRUE; else same=FALSE; fi
    echo "\"$f\",$h1,$h2,$same"
  done
} > "$ROOT/reports/reproducibility.csv"
echo "Files compared: $(($(wc -l < "$ROOT/reports/reproducibility.csv") - 1)); differing: $(grep -c ',FALSE$' "$ROOT/reports/reproducibility.csv" || true)"
