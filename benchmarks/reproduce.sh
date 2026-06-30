#!/usr/bin/env bash
#
# reproduce.sh -- re-run the exact benchmark configuration behind results/SUMMARY.md.
#
# 7 packages (5 testthat + 2 non-testthat), all four tool-modes, N=500. muttest is
# auto-skipped on the non-testthat packages (lumberjack, R.methodsS3); their deps
# and any missing source are auto-installed by the driver. Includes --setup so a
# fresh machine first installs muttest / universalmutator / comby.
#
# Long run (several hours; universalmutator dominates). Suspend is blocked for the
# duration. Run detached:
#     nohup bash benchmarks/reproduce.sh > benchmarks/results/reproduce.log 2>&1 &
#
# Extra flags are passed through to run_all.sh, e.g.:
#     bash benchmarks/reproduce.sh --no-inhibit        # don't block suspend
#     bash benchmarks/reproduce.sh --budget 100        # override the budget

set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

exec bash "$HERE/run_all.sh" \
  --setup \
  --packages prettyunits,stringr,forcats,scales,jsonlite,lumberjack,R.methodsS3 \
  --tools mutator,muttest,muttest-matched,universalmutator \
  --budget 500 \
  --out benchmarks/results/benchmark_results \
  "$@"
