# Mutation-testing tool benchmark

Compares **mutator** (this package) against two other mutation-testing tools on
real R packages:

| Tool | Language | Approach | Source |
|------|----------|----------|--------|
| **mutator** | R + C++ | AST/srcref, package-aware | this repo |
| **muttest** | R | tree-sitter, package-aware | CRAN `muttest` |
| **universalmutator** | Python | language-agnostic; **regex** text rewrites (comby structural mode also supported) | `pip install universalmutator` |

For each *tool × package* we report, capped to the **same mutant budget** so the
numbers are comparable:

- **performance**: end-to-end wall-clock (`wall_clock_s`) and `mutants_per_s`;
  mutator and muttest can be repeated with `--runs N`, in which case
  `wall_clock_s` is the bootstrap mean and `time_ci_low/high` give the 95% CI;
- **effectiveness**: `killed`, `survived`, and `mutation_score` (= killed / tested);
- **generation**: `generated_total`, the size of each tool's full mutant pool
  before capping (the basis for the discrepancy analysis).

The LLM equivalence-detection benchmark is separate from the performance/tool
comparison. It reuses mutator to build fixed survived-mutant samples, then runs
the same mutants through each CERIT/LiteLLM model and repeat.

## Targets

**testthat packages** (all three tools): the five vendored packages under
`packages/` with a real `tests/testthat/` suite: **prettyunits, stringr, forcats,
scales, jsonlite**. (`oRaklE` has a single test file, so it's excluded.)

**Non-testthat packages** (mutator + universalmutator only; muttest is testthat-only
and is auto-skipped): **lumberjack** (a **tinytest** package) and **R.methodsS3**
(a **custom raw-`tests/*.R` harness**: base-R test scripts with `stopifnot`/`stop`
assertions, no framework). Both are pure-R with green baselines here. These show the
tools work beyond testthat. (`nanotime` was the original non-testthat candidate but
is **dropped**; see below.)

## Budget & confidence

Default **N = 500** mutants per package per tool. Each tool samples its *own*
mutant pool down to N with a shared seed (`SEED` in `lib/common.R`); when a pool is
smaller than N the whole pool is used and the actual `tested_n` is recorded. The
mutation score is a sampled proportion, so a Wilson 95% CI (`score_ci_low/high`) is
reported whenever sampling occurred (N=500 ⇒ ≈ ±4.4 pp worst case, tighter for
high scores or small pools via the finite-population effect).

## Methodology: each tool at its best (documented)

All three run the **identical test suite in CRAN mode** (`NOT_CRAN` unset/`"false"`,
so `skip_on_cran()` prunes flaky/long tests), the one cross-tool consistency
override, applied so timing and kill signals are comparable.

**Consistent exclusions.** mutator honors covr's `# nocov` regions and
`.covrignore`. To keep the comparison fair, muttest and universalmutator are given
the **same file set** via `tool_source_files()` (`lib/common.R`): files matched by
`.covrignore` and whole-file `# nocov` files (e.g. `compat-*`,
`import-standalone-*`) are excluded for every tool, so all three mutate only the
"code under test." (Partial in-file `# nocov` regions are honored by mutator
internally but not re-applied to the others; rare in these targets.)

### mutator (`tools/bench_mutator.R`)
`mutate_package(work, cores = detectCores()-2, max_mutants = 500,
coverage_guided = TRUE, coverage_backend = "per_file", cran = TRUE,
max_line_deletions = 0, detectEqMutants = FALSE, timeout_seconds = 120)`.
Coverage-guided selection (only tests that cover a mutant's lines run) is
mutator's headline speedup; `per_file` is the precise attribution backend.
`max_line_deletions = 0` disables line-deletion mutants so mutator emits **only
AST operator/constant mutants**, comparable to muttest and universalmutator (line
deletions are highly killable and would otherwise inflate mutator's score).
Metrics come straight from `$summary` / `$timing`.

### muttest (`tools/bench_muttest.R`): two variants
- **`muttest (full)`**, broadest preset set: `arithmetic_operators`,
  `comparison_operators`, `logical_operators`, `boolean_literals`, `na_literals`,
  `numeric_literals`, `string_literals`, `condition_mutations`, `index_mutations`,
  `delete_statement`, `replace_return_value`. muttest at its most capable.
- **`muttest (matched)`**, restricted to the constructs mutator also mutates
  (`arithmetic` + `comparison` + `logical` + `delete_statement`), so its score is
  **directly comparable** to mutator and universalmutator. The full variant scores
  much lower because its literal/constant mutators (numbers, strings, booleans) are
  rarely killed by tests, that gap measures mutator-set breadth, not suite quality.

`muttest(plan, workers = detectCores()-2, test_strategy = default_test_strategy(),
timeout = Inf)`. Full test strategy (the faster `FileTestStrategy` trades accuracy
and is not used). muttest has no cap, so the full plan is sampled to N.

Two muttest 0.2.1 issues were found and handled (both verified against a
fresh-process-per-mutant ground truth):

1. **`timeout=Inf` is required.** muttest enforces `timeout` from task
   *submission*, not execution start, and creates all `mirai` tasks upfront. With
   many mutants queued behind `workers` daemons, queued tasks blow the timeout
   while merely *waiting* and are scored as non-kills. A finite timeout collapsed
   the score (stringr: **6.8%** at `timeout=120s`). With `timeout=Inf` the score is
   worker-count-independent (verified identical for workers ∈ {1,4,16,50}).

2. **Two rows: native vs errors-as-kills.** muttest counts a mutant killed *only*
   when an expectation **fails** (`sum(df$failed) > 0`); a mutant that makes a test
   **error/crash** is scored **survived**. mutator, universalmutator, and standard
   mutation-testing practice count crashes as kills. We therefore report two rows
   from the *same* muttest run, via a `MutationReporter` subclass (muttest's own
   extension API; runner and mutations untouched):
   - **`muttest (<variant>)`**: muttest's native score (expectation-failures only);
   - **`muttest (<variant>+err)`**: comparable score (failed **OR** errored),
     which matches the fresh-process ground truth (stringr 50-sample: native
     31/50 = 62%, errors-as-kills 41/50 = **82%** = ground truth).

   The subclass also sidesteps the progress reporter's crash when printing a
   surviving multi-line statement diff.

So muttest appears in the tables as up to four rows per package: full / matched ×
native / errors-as-kills.

### universalmutator: regex mode (`tools/bench_universalmutator.R`)
Single-file tool, orchestrated to package level: `mutate <file> r --noCheck`
every `R/` file, pool all mutants, sample N, then `analyze_mutants` each sampled
mutant with the CRAN-mode test command (**exit 0 = survived, non-zero = killed**).
Sequential analysis by design.

**Why regex, not comby.** comby (structural) mode spawns one comby process per
candidate substitution, so generating the pool for one package took **~13 min**;
regex mode produces a comparable pool in **~1.5 s** (≈1000× faster) for the same
package. comby is still wired behind `mode = "comby"`, but regex is used.

**Validity filter (important).** universalmutator's "compile" step does two jobs:
validity (drop mutants that don't compile/parse) and Trivial Compiler Equivalence
(drop mutants compiling identically to the original). For R its handler is a stub
that always returns `VALID`, and we pass `--noCheck`, so **neither runs**: textual
rewriting produces **syntactically invalid** R (e.g. `<-` → `<+`, since `- → +`
fires inside the assignment arrow) that would be killed instantly and inflate the
score. Since the AST tools only ever emit parseable mutants, we add a **parse
validity filter**: each generated mutant is `parse()`d in one R session and the
non-parseable ones are dropped before sampling. (Equivalent to universalmutator's
`mutate --cmd "Rscript -e parse(MUTANT)"`, but in-process; per-mutant Rscript
spawns would erase the regex speed advantage.) It is validity-only, not TCE dedup.
`generated_total` reports the **valid** pool; `notes` carries the raw pool size and
the count dropped as invalid.

Residual caveat: universalmutator's universal rules still produce more
trivial/redundant mutants than the AST tools (no TCE dedup), so its score is biased
high relative to mutator/muttest even after validity filtering.

### Test frameworks beyond testthat

`test_framework()` (`lib/common.R`) detects three harness types and the kill oracle
(`test_command()`) adapts; `bench_mutator.R` maps testthat to the `testthat`
strategy and tinytest to the `tinytest` strategy (both in-process, both
coverage-guided), and uses the `installed` fallback for raw `tests/*.R` layouts
(no coverage guidance). **muttest is auto-skipped** (it is hard-wired to
`testthat::test_dir`):

- **testthat** (`tests/testthat/`): `load_all` + `test_dir(stop_on_failure)`.
- **tinytest** (`inst/tinytest/`): `load_all` + `tinytest::run_test_dir`.
- **rtests** (raw `tests/*.R`, no framework): `lib/run_rtests.R` runs **each test
  file in its own fresh R process** with the package loaded (matching R CMD check,
  where files don't share state); exit non-zero if any errors. Running all files in
  one session gives spurious failures from state leakage, so per-file isolation is
  required.

All three exit non-zero ⇒ killed, so universalmutator's exit-code contract and
mutator's installed strategy agree.

## Prerequisites & setup

```bash
bash benchmarks/setup.sh
```

Installs (no root required):
- `muttest` + `treesitter.r` (CRAN), plus `remotes`, `jsonlite`, `fs`;
- `universalmutator` into `benchmarks/.venv` (PEP-668-safe);
- `comby` 1.7.0 → `~/.local/bin`, with `libev.so.4` / `libpcre.so.3` extracted
  from Debian packages → `~/.local/lib` (the benchmark sets `LD_LIBRARY_PATH`);
- dependencies of each target package (so baseline suites are green).

> setup.sh also patches universalmutator's `comby_language_for_extension` to map
> `.R`/`.r` → comby's `.generic` matcher (comby has no native R matcher).

## Running

### One-shot (recommended)

`run_all.sh` chains the whole pipeline (optional `setup` → run → baselines →
summarize) and **blocks system suspend** for the duration (auto-released on exit):

```bash
# defaults: 5 testthat packages, all tool-modes, N=500
nohup bash benchmarks/run_all.sh > benchmarks/results/run_all.log 2>&1 &
tail -f benchmarks/results/run_all.log

bash benchmarks/run_all.sh --setup                       # also install the tools
bash benchmarks/run_all.sh --packages prettyunits --budget 100   # quick subset
bash benchmarks/run_all.sh --runs 5                     # repeat mutator/muttest timings
bash benchmarks/run_all.sh --help                        # all options
```

To reproduce **exactly** the configuration behind `results/SUMMARY.md` (the 7
packages (5 testthat + lumberjack + R.methodsS3), all tool-modes, N=500, with
`--setup`):

```bash
nohup bash benchmarks/reproduce.sh > benchmarks/results/reproduce.log 2>&1 &
```

It's a long run (several hours; universalmutator dominates). Both scripts pass
extra flags through (`--no-inhibit`, `--budget`, etc.).

### Manual / per-step

```bash
# Full run: 5 packages × 4 tool-modes at N=500
# (mutator, muttest full, muttest matched, universalmutator regex)
Rscript benchmarks/run_benchmark.R

# Smoke run first (recommended): tiny budget, one package
Rscript benchmarks/run_benchmark.R --budget 30 --packages prettyunits

# Options
Rscript benchmarks/run_benchmark.R \
  --budget 500 \
  --runs 5 \
  --packages prettyunits,stringr,forcats,scales,jsonlite \
  --tools mutator,muttest,muttest-matched,universalmutator \
  --out benchmarks/results/benchmark_results

# Build the markdown result tables afterwards
Rscript benchmarks/summarize.R
```

Results are written **incrementally** to `results/benchmark_results.csv` and
`.json` (a long run is never lost). Columns: `tool, mode, package,
generated_total, tested_n, killed, survived, timed_out, mutation_score,
score_ci_low, score_ci_high, wall_clock_s, mutants_per_s, time_runs,
time_ci_low, time_ci_high, time_samples, notes`.

`--runs N` repeats the expensive timing measurement for **mutator** and
**muttest** variants only; `universalmutator` remains single-run because it is
usually the runtime bottleneck. The command-line flag takes precedence over the
`BENCH_RUNS` environment variable, and both default to `1`. With `N > 1`, the
driver keeps the same sampled mutant set per repeat, reports `wall_clock_s` as the
bootstrap mean of the repeated wall-clock samples, and stores the bootstrap 95%
interval in `time_ci_low/high`.

### LLM equivalence detection

The equivalence benchmark uses the same OpenAI-compatible configuration as
`identify_equivalent_mutants()` (`.openai_config` / `OPENAI_API_KEY`,
`OPENAI_BASE_URL`, `OPENAI_MODEL`). The current CERIT endpoint can be inspected
without printing the API key:

```bash
Rscript benchmarks/list_cerit_models.R
Rscript benchmarks/list_cerit_models.R --json
```

Run the benchmark on a fixed sample of survived mutants. Defaults are
`prettyunits,stringr,scales`, 25 survived mutants per package, 3 repeats, and the
curated CERIT chat-model list from `lib/common.R`. Jobs are randomized by default
across package/model/repeat combinations to reduce bias from network load changes;
use `--no-randomize` for a deterministic grouped order.

```bash
Rscript benchmarks/run_equivalence_benchmark.R \
  --packages prettyunits,stringr,scales \
  --models qwen3.5,deepseek-v4-pro,glm-5.2 \
  --mutants 25 \
  --repeats 3 \
  --candidate-budget 500 \
  --batch-size 25 \
  --out benchmarks/results/equivalence_benchmark

Rscript benchmarks/summarize_equivalence.R \
  benchmarks/results/equivalence_benchmark.csv
```

Raw job timings are written incrementally to `equivalence_benchmark.csv/.json`;
per-mutant classified verdicts, prompts, responses, and selected-mutant manifests
are written next to them under `*_verdicts.*` and `*_artifacts/`. The summary
reports bootstrap timing intervals, average verdict counts, and whether repeated
runs gave the same classified verdicts for the same mutant set.

**Self-contained per package.** For each `--packages` target the driver, as a first
step:
1. **fetches the source** if it's not already under `packages/`:
   `ensure_package_source()` downloads the CRAN source tarball and extracts it in
   place (skipped if the dir exists; a non-CRAN package that's absent is skipped
   with a notice);
2. **installs its dependencies** (incl. `Suggests`, which tests often need) via
   `ensure_deps()` → `remotes::install_deps(dependencies=TRUE, upgrade="never")`,
   which is idempotent (already-installed packages are left alone). Pass
   `--skip-deps` to bypass this (e.g. when your library is already complete).

So `Rscript benchmarks/run_benchmark.R --packages somePkg` works even if `somePkg`
is neither vendored nor has its deps installed, as long as it's on CRAN. This is
what `setup.sh`'s hardcoded dependency step used to cover; the driver now does it
for any target. `baseline_green()` still runs as a pre-flight and flags any package
whose suite isn't green after deps are installed.

## Results

N = 500 mutants/tool/package (sampled; fewer when a tool's pool < 500, then
exact). Full machine-generated tables are in `results/SUMMARY.md` and the raw data
in `results/benchmark_results*.csv`. **7 packages** completed, 5 testthat + 2
non-testthat (lumberjack/tinytest, R.methodsS3/custom). **nanotime was dropped**
(see below).

### Mutation score: comparable basis

muttest's *native* score counts only expectation-failures; the **errors-as-kills**
score (failed **or** errored) is the one comparable to mutator and universalmutator
(which both count crashes as kills). The headline comparison uses the comparable
basis:

| Package | harness | mutator | muttest (matched, err=kill) | universalmutator | muttest *native* (matched) |
|---|---|--:|--:|--:|--:|
| prettyunits | testthat | **88.2** | 84.3 | **93.0** | 58.5 |
| stringr | testthat | 75.4 | 71.7 | **91.4** | 37.4 |
| forcats | testthat | 70.4 | **99.7** | 89.0 | 98.6 |
| scales | testthat | 65.0 | **100.0** | 77.0 | 98.4 |
| jsonlite | testthat | 79.0 | **100.0** | 99.8 | 23.0 |
| lumberjack | tinytest | 63.0 |, (n/a) | **80.0** |, |
| R.methodsS3 | custom rtests | 43.6 |, (n/a) | **72.3** |, |

(muttest is testthat-only → not applicable to the two non-testthat packages.)

Takeaways:
- **No tool dominates.** mutator leads on prettyunits/stringr; muttest is near-perfect
  on forcats/scales/jsonlite; universalmutator is uniformly high (72–99.8%).
- **universalmutator scores high everywhere** because its rule-less textual rewrites
  are *disruptive* (identifier/operator/constant swaps that crash code → killed),
  even after the parse-validity filter.
- **mutator is lowest on forcats (70), scales (65), R.methodsS3 (44)**, its surviving
  mutants are the open question flagged below. R.methodsS3's raw smoke-style tests
  (mostly run-without-error, few value assertions) are weak, which also depresses
  mutator more than universalmutator's crash-prone mutants.
- **Both non-testthat harnesses work** for mutator (tinytest strategy for
  lumberjack, installed strategy for R.methodsS3's rtests) and universalmutator
  (tinytest / rtests oracles), demonstrating the tools generalize beyond testthat.

### muttest native vs. errors-as-kills (the kill-definition effect)

| Package | native (full) | err=kill (full) | Δ |
|---|--:|--:|--:|
| prettyunits | 59.2 | 78.6 | +19 |
| stringr | 39.8 | 68.8 | +29 |
| forcats | 98.6 | 99.8 | +1 |
| scales | 97.6 | 100.0 | +2 |
| jsonlite | **21.0** | **100.0** | **+79** |

The gap is the fraction of mutants that *crash* the package rather than fail an
assertion. It is enormous on jsonlite (JSON parsing: almost every operator mutant
throws) and small on forcats/scales (tests assert values directly). Reporting only
muttest's native score would badly misrepresent its detection ability.

### Timing (wall-clock, N=500, same machine)

| Package | mutator | muttest (full) | muttest (matched) | universalmutator |
|---|--:|--:|--:|--:|
| prettyunits | 202s | 215s | 206s | 984s |
| stringr | 232s | 622s | 610s | 1688s |
| forcats | 225s | 510s | 376s | 1561s |
| scales | 281s | 716s | 1874s¹ | 2551s |
| jsonlite | 117s | 742s | 741s | 2130s |
| lumberjack | 323s | n/a | n/a | 395s |
| R.methodsS3 | 325s | n/a | n/a | 3439s² |

- **mutator is fastest** (117–325s), parallel + coverage-guided *test selection*
  (runs only the tests covering each mutant). The recorded lumberjack numbers
  predate first-class tinytest support: they were measured under the *installed*
  strategy without coverage guidance, hence the higher 323s. tinytest now runs
  in-process (`load_all`) with coverage guidance like testthat; only the raw
  `tests/*.R` layout (R.methodsS3) still uses the *installed* strategy (reinstalls
  per mutant). Re-run to refresh the tinytest rows.
- **muttest** is 2–8× slower (parallel workers, but full suite per mutant).
- **universalmutator is 5–18× slower than mutator**, sequential, fresh R process
  per mutant.
- ¹ scales muttest-matched includes a 30-min bounded timeout: one operator mutant
  causes an **infinite loop**; with `timeout=1800s` it is killed and counted as an
  error-kill (see muttest notes above).
- ² R.methodsS3-um is the slowest cell: its **rtests** oracle runs each of the
  package's test files in a *separate* R process per mutant (R-CMD-check semantics),
  so per-mutant cost is multiplied by the test-file count.

### Discrepancy analysis: mutants generated (full pool, before capping)

| Package | mutator | muttest (full) | muttest (matched) | universalmutator |
|---|--:|--:|--:|--:|
| prettyunits | 1,086 | 1,077 | 453 | 3,925 |
| stringr | 1,260 | 1,102 | 498 | 5,014 |
| forcats | 788 | 692 | 360 | 3,412 |
| scales | 4,720 | 4,794 | 1,716 | 18,470 |
| jsonlite | 2,240 | 1,946 | 699 | 7,140 |
| lumberjack | 654 | n/a | n/a | 2,564 |
| R.methodsS3 | 1,197 | n/a | n/a | 4,116 |

(universalmutator counts are the **valid** pool after the parse filter; it discards
~25–35% non-parseable textual mutants, e.g. `<-`→`<+`, before this.)

Why the pools differ so much:
- **mutator ≈ muttest (full)** in magnitude, both are AST/tree-sitter, package-aware,
  one mutant per mutable node. mutator additionally mutates constants/`NA`/strings;
  muttest (full) adds literal/index/return mutators, hence similar totals.
- **muttest (matched)** is ~⅓–½ of full: operators + statement-deletion only.
- **universalmutator is 3–10× larger** than the AST tools: it applies the *universal*
  text rules at every textual match with **no R-aware dedup/validity** (its TCE step
  is a no-op for R), so one source construct yields many redundant/overlapping
  mutants. scales' 18,470 vs mutator's 4,720 is the clearest case.

Operator-repertoire and coverage effects:
- The **score** differences track *what* each tool mutates and *where*. mutator's
  constant→`NULL`/`NA` mutations tend to crash (killable); its coverage-guided
  population also *counts mutants on uncovered lines as SURVIVED*, which contributes
  to its lower forcats/scales scores (larger packages with more untested code).
- **Verified genuine (not a coverage-guidance artifact).** Re-running mutator with
  `coverage_guided=FALSE` (full suite per mutant = ground truth) at the same seed
  reproduces the coverage-guided scores exactly: **forcats 70.4% = 70.4%**, and
  **scales 66% = 66%** (budget-200 control, `isolate=TRUE`). So coverage-guidance
  does not manufacture false survivors here, mutator's lower scores reflect real
  test-suite gaps. (R.methodsS3's 43.6% is already ground-truth: its installed
  strategy uses no coverage-guidance, and its raw smoke-tests assert little.)
- **mutator bug found during this check:** `coverage_guided=FALSE` + `isolate=FALSE`
  (symlinked copies) on scales makes every mutant's test run fail with
  `cannot open the connection` → spurious 100% kill in ~20s. The benchmark uses
  `coverage_guided=TRUE` (which works correctly), so results are unaffected; but the
  `FALSE`+symlink path has a fixture/cwd issue worth fixing in mutator. `isolate=TRUE`
  avoids it.

### muttest reliability findings (verified)

Both surfaced during this benchmark and were handled within muttest's own API
(see the muttest methodology section):
1. **Timeout from task submission**: finite `timeout` made queued mutants
   spuriously "time out" as non-kills (stringr 6.8% at `timeout=120s`). Verified
   against a fresh-process ground truth (82%); fixed with a large finite timeout.
2. **Errors ≠ kills**: muttest scores crash-inducing mutants as *survived*; the
   errors-as-kills reporter (validated to reproduce the 82% ground truth) restores
   comparability.
