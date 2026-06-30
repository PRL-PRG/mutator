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

- **performance** — end-to-end wall-clock (`wall_clock_s`) and `mutants_per_s`;
- **effectiveness** — `killed`, `survived`, and `mutation_score` (= killed / tested);
- **generation** — `generated_total`, the size of each tool's full mutant pool
  before capping (the basis for the discrepancy analysis).

A separate experiment (not here) will measure mutator's equivalence detection.

## Targets

**testthat packages** (all three tools): the five vendored packages under
`packages/` with a real `tests/testthat/` suite — **prettyunits, stringr, forcats,
scales, jsonlite**. (`oRaklE` has a single test file, so it's excluded.)

**Non-testthat packages** (mutator + universalmutator only; muttest is testthat-only
and is auto-skipped): **lumberjack** (a **tinytest** package) and **R.methodsS3**
(a **custom raw-`tests/*.R` harness** — base-R test scripts with `stopifnot`/`stop`
assertions, no framework). Both are pure-R with green baselines here. These show the
tools work beyond testthat. (`nanotime` was the original non-testthat candidate but
is **dropped** — see below.)

## Budget & confidence

Default **N = 500** mutants per package per tool. Each tool samples its *own*
mutant pool down to N with a shared seed (`SEED` in `lib/common.R`); when a pool is
smaller than N the whole pool is used and the actual `tested_n` is recorded. The
mutation score is a sampled proportion, so a Wilson 95% CI (`score_ci_low/high`) is
reported whenever sampling occurred (N=500 ⇒ ≈ ±4.4 pp worst case, tighter for
high scores or small pools via the finite-population effect).

## Methodology — each tool at its best (documented)

All three run the **identical test suite in CRAN mode** (`NOT_CRAN` unset/`"false"`,
so `skip_on_cran()` prunes flaky/long tests) — the one cross-tool consistency
override, applied so timing and kill signals are comparable.

**Consistent exclusions.** mutator honors covr's `# nocov` regions and
`.covrignore`. To keep the comparison fair, muttest and universalmutator are given
the **same file set** via `tool_source_files()` (`lib/common.R`): files matched by
`.covrignore` and whole-file `# nocov` files (e.g. `compat-*`,
`import-standalone-*`) are excluded for every tool, so all three mutate only the
"code under test." (Partial in-file `# nocov` regions are honored by mutator
internally but not re-applied to the others — rare in these targets.)

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

### muttest (`tools/bench_muttest.R`) — two variants
- **`muttest (full)`** — broadest preset set: `arithmetic_operators`,
  `comparison_operators`, `logical_operators`, `boolean_literals`, `na_literals`,
  `numeric_literals`, `string_literals`, `condition_mutations`, `index_mutations`,
  `delete_statement`, `replace_return_value`. muttest at its most capable.
- **`muttest (matched)`** — restricted to the constructs mutator also mutates
  (`arithmetic` + `comparison` + `logical` + `delete_statement`), so its score is
  **directly comparable** to mutator and universalmutator. The full variant scores
  much lower because its literal/constant mutators (numbers, strings, booleans) are
  rarely killed by tests — that gap measures mutator-set breadth, not suite quality.

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
   extension API — runner and mutations untouched):
   - **`muttest (<variant>)`** — muttest's native score (expectation-failures only);
   - **`muttest (<variant>+err)`** — comparable score (failed **OR** errored),
     which matches the fresh-process ground truth (stringr 50-sample: native
     31/50 = 62%, errors-as-kills 41/50 = **82%** = ground truth).

   The subclass also sidesteps the progress reporter's crash when printing a
   surviving multi-line statement diff.

So muttest appears in the tables as up to four rows per package: full / matched ×
native / errors-as-kills.

### universalmutator — regex mode (`tools/bench_universalmutator.R`)
Single-file tool, orchestrated to package level: `mutate <file> r --noCheck`
every `R/` file, pool all mutants, sample N, then `analyze_mutants` each sampled
mutant with the CRAN-mode test command (**exit 0 = survived, non-zero = killed**).
Sequential analysis by design.

**Why regex, not comby.** comby (structural) mode spawns one comby process per
candidate substitution, so generating the pool for one package took **~13 min**;
regex mode produces a comparable pool in **~1.5 s** (≈1000× faster) for the same
package. comby is still wired behind `mode = "comby"`, but regex is used.

**Validity filter (important).** universalmutator's "compile" step does two jobs —
validity (drop mutants that don't compile/parse) and Trivial Compiler Equivalence
(drop mutants compiling identically to the original). For R its handler is a stub
that always returns `VALID`, and we pass `--noCheck`, so **neither runs**: textual
rewriting produces **syntactically invalid** R (e.g. `<-` → `<+`, since `- → +`
fires inside the assignment arrow) that would be killed instantly and inflate the
score. Since the AST tools only ever emit parseable mutants, we add a **parse
validity filter**: each generated mutant is `parse()`d in one R session and the
non-parseable ones are dropped before sampling. (Equivalent to universalmutator's
`mutate --cmd "Rscript -e parse(MUTANT)"`, but in-process — per-mutant Rscript
spawns would erase the regex speed advantage.) It is validity-only, not TCE dedup.
`generated_total` reports the **valid** pool; `notes` carries the raw pool size and
the count dropped as invalid.

**Not coverage-guided (deliberately).** mutator's `coverage_guided` is only a
test-*selection* speedup — it does not change verdicts; a mutant on an uncovered
line is still counted as **SURVIVED**. So mutator's denominator is *all* mutable
lines (minus nocov/.covrignore files). For universalmutator to match that
population it must mutate **all** lines too (uncovered-line mutants then survive,
as in mutator). Restricting universalmutator to covered lines would drop exactly
those survivors and inflate its score — and brings no speed benefit (covr overhead;
the analyzed count is capped at N regardless). A `coverage_guided` option remains in
the wrapper (`mutate --lines <covered>`) but is **off** for the benchmark.

Residual caveat: universalmutator's universal rules still produce more
trivial/redundant mutants than the AST tools (no TCE dedup), so its score is biased
high relative to mutator/muttest even after validity filtering.

### Test frameworks beyond testthat

`test_framework()` (`lib/common.R`) detects three harness types and the kill oracle
(`test_command()`) adapts; mutator auto-selects its **installed** strategy for any
non-testthat package (coverage-guided is testthat-only), and **muttest is
auto-skipped** (it is hard-wired to `testthat::test_dir`):

- **testthat** (`tests/testthat/`) — `load_all` + `test_dir(stop_on_failure)`.
- **tinytest** (`inst/tinytest/`) — `load_all` + `tinytest::run_test_dir`.
- **rtests** (raw `tests/*.R`, no framework) — `lib/run_rtests.R` runs **each test
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

```bash
# Full run: 5 packages × 4 tool-modes at N=500
# (mutator, muttest full, muttest matched, universalmutator regex)
Rscript benchmarks/run_benchmark.R

# Smoke run first (recommended): tiny budget, one package
Rscript benchmarks/run_benchmark.R --budget 30 --packages prettyunits

# Options
Rscript benchmarks/run_benchmark.R \
  --budget 500 \
  --packages prettyunits,stringr,forcats,scales,jsonlite \
  --tools mutator,muttest,muttest-matched,universalmutator \
  --out benchmarks/results/benchmark_results

# Build the markdown result tables afterwards
Rscript benchmarks/summarize.R
```

Results are written **incrementally** to `results/benchmark_results.csv` and
`.json` (a long run is never lost). Columns: `tool, mode, package,
generated_total, tested_n, killed, survived, timed_out, mutation_score,
score_ci_low, score_ci_high, wall_clock_s, mutants_per_s, notes`.

**Self-contained per package.** For each `--packages` target the driver, as a first
step:
1. **fetches the source** if it's not already under `packages/` —
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
in `results/benchmark_results*.csv`. The 5 testthat packages completed; **nanotime
was dropped** (see below).

### Mutation score — comparable basis

muttest's *native* score counts only expectation-failures; the **errors-as-kills**
score (failed **or** errored) is the one comparable to mutator and universalmutator
(which both count crashes as kills). The headline comparison uses the comparable
basis:

| Package | mutator | muttest (matched, err=kill) | universalmutator | muttest *native* (matched) |
|---|--:|--:|--:|--:|
| prettyunits | **88.2** | 84.3 | **93.0** | 58.5 |
| stringr | 75.4 | 71.7 | **91.4** | 37.4 |
| forcats | 70.4 | **99.7** | 89.0 | 98.6 |
| scales | 65.0 | **100.0** | 77.0 | 98.4 |
| jsonlite | 79.0 | **100.0** | 99.8 | 23.0 |

Takeaways:
- **No tool dominates.** mutator leads on prettyunits/stringr; muttest is near-perfect
  on forcats/scales/jsonlite; universalmutator is uniformly high (77–99.8%).
- **universalmutator scores high everywhere** because its rule-less textual rewrites
  are *disruptive* (identifier/operator/constant swaps that crash code → killed),
  even after the parse-validity filter.
- **mutator is lowest on forcats (70) and scales (65)** — its surviving mutants are
  the open question flagged below.

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

- **mutator is fastest** (117–281s) — parallel + coverage-guided *test selection*
  (runs only the tests covering each mutant), and it scales well to large packages.
- **muttest** is 2–8× slower (parallel workers, but full suite per mutant).
- **universalmutator is 5–18× slower than mutator** — sequential, fresh R process
  per mutant.
- ¹ scales muttest-matched includes a 30-min bounded timeout: one operator mutant
  causes an **infinite loop**; with `timeout=1800s` it is killed and counted as an
  error-kill (see muttest notes above).

### Discrepancy analysis — mutants generated (full pool, before capping)

| Package | mutator | muttest (full) | muttest (matched) | universalmutator |
|---|--:|--:|--:|--:|
| prettyunits | 1,086 | 1,077 | 453 | 3,925 |
| stringr | 1,260 | 1,102 | 498 | 5,014 |
| forcats | 788 | 692 | 360 | 3,412 |
| scales | 4,720 | 4,794 | 1,716 | 18,470 |
| jsonlite | 2,240 | 1,946 | 699 | 7,140 |

(universalmutator counts are the **valid** pool after the parse filter; it discards
~25–35% non-parseable textual mutants — e.g. `<-`→`<+` — before this.)

Why the pools differ so much:
- **mutator ≈ muttest (full)** in magnitude — both are AST/tree-sitter, package-aware,
  one mutant per mutable node. mutator additionally mutates constants/`NA`/strings;
  muttest (full) adds literal/index/return mutators — hence similar totals.
- **muttest (matched)** is ~⅓–½ of full: operators + statement-deletion only.
- **universalmutator is 3–10× larger** than the AST tools: it applies the *universal*
  text rules at every textual match with **no R-aware dedup/validity** (its TCE step
  is a no-op for R), so one source construct yields many redundant/overlapping
  mutants. scales' 18,470 vs mutator's 4,720 is the clearest case.

Operator-repertoire and coverage effects:
- The **score** differences track *what* each tool mutates and *where*. mutator's
  constant→`NULL`/`NA` mutations tend to crash (killable), but its coverage-guided
  population also *counts mutants on uncovered lines as SURVIVED* — a likely
  contributor to its lower forcats/scales scores (large packages with more
  untested code).
- **Open item (not yet verified):** whether mutator's forcats/scales survivors are
  genuine survivors or artifacts of `coverage_guided` attribution (a covr
  under-attributed line would be auto-marked SURVIVED). muttest's low scores were
  verified against a fresh-process ground truth (below); the symmetric check for
  mutator is still TODO.

### muttest reliability findings (verified)

Both surfaced during this benchmark and were handled within muttest's own API
(see the muttest methodology section):
1. **Timeout from task submission** — finite `timeout` made queued mutants
   spuriously "time out" as non-kills (stringr 6.8% at `timeout=120s`). Verified
   against a fresh-process ground truth (82%); fixed with a large finite timeout.
2. **Errors ≠ kills** — muttest scores crash-inducing mutants as *survived*; the
   errors-as-kills reporter (validated to reproduce the 82% ground truth) restores
   comparability.

### nanotime (dropped)

nanotime was intended as a non-testthat (tinytest) data point. Its `tinytest` suite
does **not pass in this environment** even when the package is installed and run the
intended way (`tinytest::test_package`): it errors with `as.nanoduration: too many
arguments`, a dependency version skew (`bit64`/`integer64`) independent of any
mutation tool. With no green baseline, mutation kills can't be distinguished from a
broken suite, so nanotime is excluded. The tinytest harness itself
(`test_command()` + `mutate_package` installed strategy) is wired and would apply to
a green tinytest package; muttest cannot participate regardless (testthat-only).
