# app/ — executable entry point and CLI

Loaded when you work under `app/`. Root rules still apply: never launch
a window (see the root `CLAUDE.md` §Launch rules).

## Boot modes and flags

`app/Main.hs` selects exactly one of six boot modes from argv, in this
precedence when more than one selector is present:
`--language-report` > `--dump` > `--preview` > `--offscreen` >
`--headless` > graphical (the default, no selector needed). Every
ancillary flag below is honoured only by the mode(s) listed — passing it
to any other mode exits 1 before any engine, window, or server starts,
naming both the flag and the selected mode (CH-58).
`--resource-root <path>` (or `SYNARCHY_ROOT`) is the one global flag:
it applies to and is validated before every mode.

| Flag | Honoured by |
|---|---|
| `--seed`, `--worldSize`, `--plates` (alias `--ages`), `--region` | `--dump` |
| `--size` | `--offscreen` |
| `--seeds` | `--language-report` |
| `--arena` | `--headless`, `--offscreen`, graphical |
| `--port` | `--headless`, `--offscreen`, `--preview`, graphical |

The rejection table lives in `app/Main.hs`'s `incompatibleFlagTable`;
`tools/preview_cli_probe.py` is the no-boot gate covering it.

**A present-but-malformed value is an error, not a default (#1191).** In
a mode that honours it, a non-numeric
`--seed`/`--worldSize`/`--plates`/`--ages`/`--port`, a `--size` that isn't
`WxH` with both dimensions positive, and a `--dump=` selection that is
empty or names an unknown layer each exit 1 pre-boot naming the flag and
the offending token. **Omitting** a flag still keeps its documented
default — only a value the user actually typed can fail. `--region` is
deliberately excluded (`docs/code_health_findings.md` CH-67). Ordering
against the mode-compatibility rejection, and the full token rules, are in
`docs/engine_contracts.md` §CLI value validation. Gates: hspec
`--match "App.Cli"`, `tools/preview_cli_probe.py`.

## Resource root (#636)

Every runtime resource family (`scripts/`, `assets/`, `data/`,
`config/`) is loaded by cwd-relative paths. The executable resolves ONE
resource root at startup (`App.ResourceRoot`) and chdirs into it.
Precedence: `--resource-root <path>` flag > `SYNARCHY_ROOT` env var >
cwd. The root is validated before dispatch (missing root/family exits 1
with a clear error). The chdir means relative OUTPUT paths (`saves/`,
config saves) also land under the resource root. Gate:
`tools/resource_root_probe.py` (manual-only).

## Console listener policy (#1190)

`--headless`/`--offscreen` have no window, so a listener that cannot
start (occupied or unbindable port, or `--port 0`) ABORTS the boot:
non-zero exit, no `READY` marker, cause on stderr, partial boot torn
down. `--dump`, graphical and `--preview` keep their tolerance. Contract:
`docs/engine_contracts.md` §Debug-console listener policy. Gates: hspec
`--match "debug-console listener policy"`,
`tools/debug_console_boot_probe.py`.

## Preview mode (`App.Preview`, `BootPreview`)

A real GLFW window + Vulkan with no world/unit/sim/combat thread,
booting straight to `scripts/preview_manager.lua`. Pre-boot rejection is
the load-bearing rule: an unknown name, path structure or traversal, a
symlinked directory, or a file where a directory was expected all exit 1
before a window exists. The category set
(`App.Cli.classifyPreviewCategory`), trimmed loading, both viewers, the
dump contract and centered bounded zoom: `docs/engine_contracts.md`
§Preview mode. Gates: `tools/preview_cli_probe.py` (CI-eligible, no
boot), `tools/preview_probe.py` (manual-only, `needs-gpu`), hspec
`--match "Preview.Discovery"` / `"Preview.UnitAnimation"` /
`"Preview.Building"` / `"Preview.Zoom"`.
