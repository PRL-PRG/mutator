#!/usr/bin/env bash
#
# setup.sh -- install the tools and dependencies needed to run the
# mutation-testing benchmark (mutator vs. muttest vs. universalmutator).
#
# Idempotent: re-running skips anything already present. No root required;
# everything lands under the benchmark dir (.venv) or $HOME/.local.
#
# Installs:
#   1. muttest + treesitter.r   (CRAN, for the muttest tool)
#   2. universalmutator         (pip, into benchmarks/.venv -> mutate/analyze_mutants)
#   3. comby                     (binary -> ~/.local/bin, + libev/libpcre -> ~/.local/lib)
#   4. dependencies of the       (so their test suites are green)
#      target benchmark packages
#
# comby is dynamically linked against libev.so.4 and libpcre.so.3. On systems
# lacking those (and without sudo) we extract them from Debian .deb packages into
# ~/.local/lib; the benchmark sets LD_LIBRARY_PATH accordingly when it runs comby.

set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$HERE/.." && pwd)"
PKG_DIR="$REPO_ROOT/packages"
VENV="$HERE/.venv"
LOCAL_BIN="$HOME/.local/bin"
LOCAL_LIB="$HOME/.local/lib"

TARGET_PKGS=(prettyunits stringr forcats scales jsonlite)

log() { printf '\n\033[1;34m==> %s\033[0m\n' "$*"; }

mkdir -p "$LOCAL_BIN" "$LOCAL_LIB"

# ---------------------------------------------------------------------------
# 1. muttest + treesitter.r (+ helpers used by the wrappers)
# ---------------------------------------------------------------------------
log "Installing R packages: muttest, treesitter.r, remotes, jsonlite, fs"
Rscript -e '
  need <- c("muttest", "treesitter.r", "remotes", "jsonlite", "fs")
  miss <- setdiff(need, rownames(installed.packages()))
  if (length(miss)) install.packages(miss, repos = "https://cloud.r-project.org")
  ok <- vapply(need, requireNamespace, logical(1), quietly = TRUE)
  if (!all(ok)) stop("Failed to install: ", paste(need[!ok], collapse = ", "))
  cat("muttest", as.character(packageVersion("muttest")), "ready.\n")
'

# ---------------------------------------------------------------------------
# 2. universalmutator (Python, in a dedicated venv to avoid PEP-668 issues)
# ---------------------------------------------------------------------------
if [ -x "$VENV/bin/mutate" ] && [ -x "$VENV/bin/analyze_mutants" ]; then
  log "universalmutator already installed in $VENV"
else
  log "Creating venv and installing universalmutator"
  python3 -m venv "$VENV"
  "$VENV/bin/pip" install --quiet --upgrade pip
  "$VENV/bin/pip" install --quiet universalmutator
fi
"$VENV/bin/mutate" --help >/dev/null 2>&1 && echo "mutate OK ($VENV/bin/mutate)"

# comby has no native R matcher; map .R/.r -> .generic so comby mode works.
# (Only affects --comby; regex mode is unaffected.) Idempotent.
GENMUT="$(echo "$VENV"/lib/python*/site-packages/universalmutator/genmutants.py)"
if [ -f "$GENMUT" ] && ! grep -q '".tact", ".fc", ".func", ".tolk", ".R", ".r"' "$GENMUT"; then
  sed -i 's/\[".tact", ".fc", ".func", ".tolk"\]/[".tact", ".fc", ".func", ".tolk", ".R", ".r"]/' "$GENMUT" \
    && echo "patched universalmutator comby extension map for R"
fi

# ---------------------------------------------------------------------------
# 3. comby (binary + its shared-library deps, all no-root)
# ---------------------------------------------------------------------------
COMBY_VER="1.7.0"
if [ ! -x "$LOCAL_BIN/comby" ]; then
  log "Downloading comby $COMBY_VER -> $LOCAL_BIN/comby"
  curl -sL "https://github.com/comby-tools/comby/releases/download/${COMBY_VER}/comby-${COMBY_VER}-x86_64-linux" \
    -o "$LOCAL_BIN/comby"
  chmod +x "$LOCAL_BIN/comby"
fi

# Ensure libev.so.4 and libpcre.so.3 are available (extract from Debian debs).
fetch_lib() {  # $1 = pool path, $2 = deb filename, $3 = so glob
  local so; so="$(ls "$LOCAL_LIB"/$3 2>/dev/null | head -1 || true)"
  [ -n "$so" ] && return 0
  log "Fetching $2 for $3"
  local tmp; tmp="$(mktemp -d)"
  curl -sL "http://deb.debian.org/debian/pool/main/$1/$2" -o "$tmp/$2"
  dpkg-deb -x "$tmp/$2" "$tmp/x"
  find "$tmp/x" -name "$3" -exec cp {} "$LOCAL_LIB/" \;
  rm -rf "$tmp"
}
if ! LD_LIBRARY_PATH="$LOCAL_LIB" "$LOCAL_BIN/comby" -version >/dev/null 2>&1; then
  fetch_lib "libe/libev"  "libev4t64_4.33-2.1+b3_amd64.deb" "libev.so*"
  fetch_lib "p/pcre3"     "libpcre3_8.39-15_amd64.deb"      "libpcre*.so*"
fi
if LD_LIBRARY_PATH="$LOCAL_LIB" "$LOCAL_BIN/comby" -version >/dev/null 2>&1; then
  echo "comby OK ($(LD_LIBRARY_PATH=$LOCAL_LIB "$LOCAL_BIN/comby" -version))"
else
  echo "WARN: comby still not runnable; install libev/libpcre manually" >&2
fi

# ---------------------------------------------------------------------------
# 4. Dependencies of the target packages (so baseline suites pass)
# ---------------------------------------------------------------------------
for pkg in "${TARGET_PKGS[@]}"; do
  if [ -d "$PKG_DIR/$pkg" ]; then
    log "Installing dependencies of $pkg"
    Rscript -e "remotes::install_deps('$PKG_DIR/$pkg', dependencies = TRUE, upgrade = 'never')" \
      || echo "WARN: some deps for $pkg may be missing" >&2
  fi
done

log "Setup complete."
echo "Tools:"
echo "  muttest:          Rscript -e 'library(muttest)'"
echo "  universalmutator: $VENV/bin/{mutate,analyze_mutants}"
echo "  comby:            $LOCAL_BIN/comby  (needs LD_LIBRARY_PATH=$LOCAL_LIB)"
