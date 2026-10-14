# HLint pilot

The initial configuration is on local branch `tooling/hlint-initial`, commit
`1d8669acb`, in the `hlint-initial` worktree. It is an advisory pilot, not a
required CI gate. No Haskell source was rewritten. This document records the
pilot before its tooling is merged.

## Installation and commands

Use **HLint 3.10** as a standalone development executable. This was the latest
[upstream release](https://github.com/ndmitchell/hlint/releases/tag/v3.10)
when checked on 2026-10-13, and its parser handles Synarchy's GHC2024/GHC 9.12.2
syntax. The native Apple Silicon Homebrew bottle was installed with
`HOMEBREW_NO_AUTO_UPDATE=1 brew install hlint`; the installed executable is
`/opt/homebrew/bin/hlint`. `.hlint-version` pins what the runner accepts.
Homebrew's formula may advance later; check `hlint --numeric-version`.

HLint is not a Synarchy library dependency and belongs in neither its
`build-depends` nor its normal build. An alternative pinned installation,
using Cabal to install a separate executable, is:

```sh
cabal install hlint-3.10 --ignore-project --install-method=copy --installdir="$HOME/.local/bin"
```

That is an installation alternative, not a command used for this pilot.
Use a compiler supported by that HLint release; the installation's dependency
plan is separate from Synarchy's. The HLint executable can parse project code
without building the game or loading its libraries.

From the pilot worktree:

```sh
bash tools/hlint.sh
bash tools/hlint.sh --json > /tmp/synarchy-hlint.json
bash tools/hlint.sh --report=/tmp/synarchy-hlint.html
bash tools/hlint.sh --cpp-define=DEVELOPMENT --json > /tmp/synarchy-hlint-dev.json
python3 tools/test_hlint.py
```

The runner changes to its own repository root, checks the version, runs four
threads, and selects tracked `.hs`/`.lhs` files through `--git`. Untracked new
files are not included: stage them before the sweep, or run `hlint path/to/File.hs`
from the repository root. Darwin defines `DARWIN`; `DEVELOPMENT` is opt-in.
Hints retain HLint's nonzero exit status. The baseline runs exited 1 because
of hints, not a parser/tool failure. Use HTML and JSON output in separate
commands: HLint 3.10 prioritizes JSON when both are requested.

## Initial policy

All standard hints remain enabled. `.hlint.yaml` supplies the common Cabal
language extensions and fixities for UPrelude's local and re-exported Unicode
operators. Keep these synchronized with `synarchy.cabal` and `src/UPrelude.hs`.

- Prefer `tshow` for strict `Data.Text.pack (show x)` and ASCII/Unicode
  compositions. Preserve `UPrelude.tshow` itself and the independent reference
  expressions in `Test.Headless.UPrelude.spec`. ByteString and lazy Text are
  deliberately not matched. Callers may need to import `tshow`.
- Restrict `unsafePerformIO` to seven existing bindings: five production
  counters/locks/stores and two headless-test helpers. The YAML names each
  binding; another binding in the same module is still reported. This records
  existing uses without certifying their safety. `unsafeDupablePerformIO` and
  `unsafeInterleaveIO` have no exceptions.
- Restrict `Debug.Trace` imports; use engine logging for persistent diagnostics.

No blanket exception rewrite is appropriate. `Engine.Core.Monad` implements
`MonadError` with `throwError`/`catchError`; IO boundaries use `try`, `catch`,
`throwIO`, and resource combinators. `Engine.Core.ConfigWrite.trySynchronous`
rethrows asynchronous exceptions, while worker supervision deliberately
observes broader failures. The older `tryEngine`, `throwEngineException`, and
`catchEngine` wrappers were removed. These behaviors cannot be replaced by a
single syntactic preference for `try`.

The seven Python checks exercise the real HLint CLI/configuration with temporary
Haskell snippets, covering useful matches, false-positive boundaries, exact
unsafe-binding scope, reference-test exceptions, and retained default hints.
They require no game build.

## Initial baseline — 2026-10-13

Source snapshot: `0482a98ef`; configuration: `1d8669acb`. Both macOS production
and development CPP passes scanned **1,505 tracked Haskell files** and returned
**1,213 hints** each: **696 warnings**, **517 suggestions**, **zero parse errors**.
485 real file paths have findings; two additional hints have HLint's `no-span`
location and identify `World.Render.SideDecoQuads.waterSideFaceQuads` by name.

| Hint | Count per pass |
|---|---:|
| Use uncurry | 195 |
| Eta reduce | 137 |
| Redundant bracket | 123 |
| Use tshow | 106 |
| Parenthesize unary negation | 53 |
| Avoid lambda | 50 |
| Use tuple-section | 41 |
| Use unless | 39 |
| Use fromMaybe | 37 |
| Replace case with maybe | 30 |

The 106 `tshow` findings comprise 95 in `test-headless/` and 11 in
`app-save-codec/Main.hs`; none are in `src/`. There are zero restricted-function
findings outside the seven recorded exceptions and zero `Debug.Trace` import
findings. Examples are `app-save-codec/Main.hs:249` and
`test-headless/Test/Headless/Asset/TextureFallback.hs:685`.

Full production/development JSON, a filterable HTML rendering of the production
JSON, and machine-readable counts/checksums are deliberately retained locally at
`~/work/synarchy-hlint-reports/20261013/` (`production.json`, `development.json`,
`report.html`, `summary.json`). These are generated evidence, outside the source
checkout; the HTML is a rendering of HLint JSON, not HLint's built-in report.

These are suggestions, not a defect count. Some built-in hints propose ASCII
operators that differ from project conventions; others, including `Use uncurry`
and `Use newtype instead of data`, carry laziness-change notes. HLint does not
type-check or resolve the complete import/re-export graph. Do not apply the
report wholesale. The macOS profiles do not establish coverage of every
possible platform/CPP configuration.

A useful next stage is to review the 106 `tshow` sites, curate the noisy default
rules, then integrate the accepted rules with `static-audits`, the local CI
command and its parity check. No CI gate or source cleanup is part of this pilot.
