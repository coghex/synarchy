# Using the `synarchy-deps` image outside GitHub Actions (#2864)

`ghcr.io/coghex/synarchy-deps` holds everything a synarchy build needs
except synarchy itself: Ubuntu 24.04's apt build dependencies, GHC 9.12.2
and cabal 3.16.1.0 (installed through ghcup, without GHC's profiling
libraries or bundled docs), the Hackage index snapshot, and the compiled
Cabal store for one resolved dependency plan. Unpacked on an Ubuntu 24.04
x86_64 machine, it turns a cold `cabal build all` (about 17 minutes of
dependency builds) into a build of synarchy alone.

Scope: Linux x86_64, Ubuntu 24.04, with the image's exact toolchain at its
absolute paths. The store is built natively on 24.04 because Cabal's unit
IDs include the system pkg-config versions of `vulkan` and `zlib`; on any
other Ubuntu, those packages and everything depending on them rebuild.

## Tags

| Tag | Meaning |
|---|---|
| `master` | The image for master's current dependency plan. Moves when the plan changes, including back to an earlier image after a rollback. |
| `ubuntu24.04-x86_64-ghc9.12.2-<sha256>` | Immutable: one image per resolved plan, pushed once and never overwritten. `<sha256>` is the sha256 of the producer's `plan.json`. |

The immutable tag is not an Actions cache key. CI's cache resolves its
plan inside the 22.04 CI image, so its hashes differ.

`.github/workflows/deps-image.yml` publishes on pushes to master only;
pull requests never publish. An unchanged plan publishes nothing.

## Install without Docker

From a synarchy checkout, as the user who will build:

```bash
sudo apt-get install -y --no-install-recommends \
  bash build-essential ca-certificates curl git glslang-tools gzip \
  libffi-dev libgl1-mesa-dev libgmp-dev libncurses-dev libvulkan-dev \
  libxcursor-dev libxi-dev libxinerama-dev libxrandr-dev libxxf86vm-dev \
  locales pkg-config python3 python3-pip tar xorg-dev xz-utils zlib1g-dev
sudo locale-gen en_US.UTF-8
# Both prefixes must be empty or absent, and owned by the building user,
# because cabal writes under CABAL_DIR.
sudo install -d -o "$(id -u)" -g "$(id -g)" /usr/local/.ghcup /usr/local/cabal
python3 tools/ci_deps_image.py fetch       # ghcr.io/coghex/synarchy-deps:master
export PATH=/usr/local/.ghcup/bin:$PATH CABAL_DIR=/usr/local/cabal
export LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8
```

As root, skip the `install -d` line.

The apt list is the image's own (`.github/deps/Dockerfile`, which a test
keeps identical to the CI image's). The two `pkg-config` packages
`libvulkan-dev` and `zlib1g-dev` must be 24.04's, or the store's unit IDs
will not match. The en_US.UTF-8 locale is needed by the Template Haskell
shader splice.

`fetch` uses the registry API anonymously, the same way `docker pull`
does. It gets a pull token from `https://ghcr.io/token`, reads the
manifest for the tag, and downloads the config and each layer blob,
checking every sha256 digest. Then it applies the layers in order. It
writes only `/usr/local/.ghcup` and `/usr/local/cabal`, and it honours the
layers' whiteout and opaque-directory markers within them, so the result
is the image's final content for those two directories. Nothing else of
the image is written. In particular, its Ubuntu base never replaces the
host's operating system. `fetch` refuses to unpack over non-empty
prefixes. Options: `--ref` takes another tag or `@sha256:` digest, and
`--root DIR` unpacks under another root. Unpacking elsewhere is only
useful for inspection, because GHC's install records its absolute prefix.

## Install with Docker

```bash
docker pull ghcr.io/coghex/synarchy-deps:master
docker run --rm -it -v "$PWD:/work" -w /work ghcr.io/coghex/synarchy-deps:master bash
```

The image already sets `PATH`, `CABAL_DIR` and the locale.

## Check that the store matches your checkout

The image records what it was built from in
`/usr/local/cabal/synarchy-deps.json` (and in image labels):

- the producer commit and the workflow run that built it;
- the immutable `ubuntu:24.04` base digest;
- the GHC, Cabal and platform versions;
- the index snapshot timestamp;
- every non-local unit ID of its plan.

1. Resolve your plan, then compare it:

   ```bash
   cabal build all --dry-run
   python3 tools/ci_deps_image.py check    # exit 0: MATCH, exit 1: MISMATCH
   ```

   `check` compares the compiler, the platform and the non-local unit IDs.
   Those contain no local path, so the check works from any checkout
   directory. Do not compare `plan.json` hashes: a plan records your
   checkout's absolute paths. A mismatch lists the units cabal would build
   itself. That is a slower build, not a broken one.
2. The dry-run output above must list only synarchy's own components
   (`synarchy-0.1.0.0 (...)`) and no dependency package.
3. `cabal build all` then compiles synarchy only. The store also holds
   both test suites' dependencies, so `cabal build synarchy-test-headless`
   builds no dependencies either.

A checkout of the producer commit always matches. A later commit matches
as long as its dependency plan is unchanged.

If cabal reports `The index-state is set to ..., but the latest index-state
is ...`, your checkout pins a newer `index-state` than the image's index
snapshot. That happens when the pin moved but the resolved plan did not,
so no new image was published. Run `cabal update` first, then resolve and
check again.

## Visibility

The package must be public for anonymous pulls. The publishing workflow
fails, naming the settings page, while it is not.
