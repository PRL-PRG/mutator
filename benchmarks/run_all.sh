#!/usr/bin/env bash
#
# run_all.sh -- one-shot benchmark pipeline: (setup) -> run -> baselines -> summarize.
#
# Blocks system suspend for the whole run (idle/lid), auto-released on exit.
# Run it detached for long runs:
#     nohup bash benchmarks/run_all.sh > benchmarks/results/run_all.log 2>&1 &
#     tail -f benchmarks/results/run_all.log
#
# Options (all optional):
#   --packages a,b,c   packages to benchmark        (default: 5 testthat targets)
#   --tools t1,t2      tools to run                 (default: all four)
#   --budget N         mutants/tool/package         (default: 500)
#   --runs N           timing repeats for mutator/muttest (default: BENCH_RUNS or 1)
#   --out PREFIX       output path prefix           (default: results/benchmark_results)
#   --setup            run setup.sh first (installs muttest/universalmutator/comby)
#   --skip-deps        skip per-package dependency auto-install
#   --no-inhibit       do not block suspend
#   -h, --help         show this help

set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$HERE/.." && pwd)"
cd "$REPO_ROOT"

PACKAGES="prettyunits,stringr,forcats,scales,jsonlite"
TOOLS="mutator,muttest,muttest-matched,universalmutator"
BUDGET=500
RUNS="${BENCH_RUNS:-1}"
OUT="benchmarks/results/benchmark_results"
DO_SETUP=0
SKIP_DEPS=""
INHIBIT=1

while [ $# -gt 0 ]; do
  case "$1" in
    --packages)  PACKAGES="$2"; shift 2 ;;
    --tools)     TOOLS="$2"; shift 2 ;;
    --budget)    BUDGET="$2"; shift 2 ;;
    --runs)      RUNS="$2"; shift 2 ;;
    --out)       OUT="$2"; shift 2 ;;
    --setup)     DO_SETUP=1; shift ;;
    --skip-deps) SKIP_DEPS="--skip-deps"; shift ;;
    --no-inhibit) INHIBIT=0; shift ;;
    -h|--help)   awk 'NR==1{next} !/^#/{exit} {sub(/^# ?/,"");print}' "${BASH_SOURCE[0]}"; exit 0 ;;
    *) echo "unknown option: $1" >&2; exit 2 ;;
  esac
done

log() { printf '\n\033[1;34m==> [%s] %s\033[0m\n' "$(date +%H:%M:%S)" "$*"; }

# --- block suspend for the lifetime of this script ------------------------
INH_PID=""
if [ "$INHIBIT" = 1 ] && command -v systemd-inhibit >/dev/null 2>&1; then
  systemd-inhibit --what=sleep:idle:handle-lid-switch --who="mutator-benchmark" \
    --why="run_all.sh benchmark" --mode=block \
    bash -c "while kill -0 $$ 2>/dev/null; do sleep 30; done" &
  INH_PID=$!
  log "suspend blocked (inhibitor pid $INH_PID); releases on exit"
fi
cleanup() { [ -n "$INH_PID" ] && kill "$INH_PID" 2>/dev/null; }
trap cleanup EXIT INT TERM

START=$(date +%s)

# --- pipeline -------------------------------------------------------------
if [ "$DO_SETUP" = 1 ]; then
  log "setup.sh (installing tools + deps)"
  bash benchmarks/setup.sh
fi

log "running benchmark: packages=$PACKAGES tools=$TOOLS budget=$BUDGET runs=$RUNS"
Rscript benchmarks/run_benchmark.R \
  --packages "$PACKAGES" --tools "$TOOLS" --budget "$BUDGET" --runs "$RUNS" --out "$OUT" $SKIP_DEPS \
  || { echo "benchmark run failed" >&2; exit 1; }

log "measuring plain (no-covr) baselines"
Rscript benchmarks/measure_baselines.R || echo "WARN: baseline measurement failed (× baseline columns will be omitted)" >&2

log "summarizing"
Rscript benchmarks/summarize.R "$OUT.csv" >/dev/null \
  || { echo "summarize failed" >&2; exit 1; }

log "rendering summary notebook + plots"
RESULTS_DIR=$(dirname "$OUT.csv")
BENCH_RESULTS_DIR="$RESULTS_DIR" \
  Rscript -e 'rmarkdown::render("benchmarks/benchmark_summary.Rmd", quiet=TRUE)' \
  || echo "WARN: notebook render failed (tables/plots not regenerated)" >&2

ELAPSED=$(( $(date +%s) - START ))
log "DONE in $((ELAPSED/60))m $((ELAPSED%60))s"
echo "  results : $OUT.csv / .json"
echo "  summary : benchmarks/results/SUMMARY.md  +  summary_headline.csv"
echo "  notebook: benchmarks/benchmark_summary.nb.html  (plots in $RESULTS_DIR)"
