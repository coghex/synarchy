#!/usr/bin/env python3
"""Publish and consume ghcr.io/coghex/synarchy-deps (#2864).

The image is Ubuntu 24.04 with the CI image's apt build dependencies, GHC
and cabal at the CI image's prefixes (`/usr/local/.ghcup`,
`CABAL_DIR=/usr/local/cabal`), the Hackage index snapshot and the compiled
Cabal store for one resolved dependency plan. `.github/deps/Dockerfile`
builds it; `.github/workflows/deps-image.yml` drives this tool on master
pushes; `docs/deps_image.md` is the consumer's guide.

Identity
--------
The publication identity is the dependency plan the producer resolves on
Ubuntu 24.04: the sha256 of `plan.json`, resolved at the fixed workspace
`/src/synarchy` with `CABAL_DIR=/usr/local/cabal` so that the absolute paths
plan.json records are the same on every run. Its immutable tag carries a
platform marker, `ubuntu24.04-x86_64-ghc9.12.2-<sha256>`, so it cannot be
confused with an Actions cache key (`deps-v2-Linux-...`, resolved inside the
22.04 CI image, whose plan differs). An unseen identity is built and pushed
exactly once; a seen one is never rebuilt or overwritten, so it keeps its
original provenance; `master` names master's current identity, so an
A -> B -> A rollback reuses A and moves `master` back to it.

Publication rules (`publish`, `reconcile-master`)
-------------------------------------------------
* Only a push to `refs/heads/master` of coghex/synarchy writes anything.
  Every other event is refused before the first registry request,
  same-repository pull requests included.
* "Absent" means the registry said so: 404 with MANIFEST_UNKNOWN or
  NAME_UNKNOWN. Anything else (rate limit, 5xx, denied, an unparsable 404)
  fails the job instead of guessing, because a wrong "absent" would rebuild
  and overwrite a published tag.
* `publish` pushes the immutable tag only. The workflow serializes runs of
  one identity with a job-level `concurrency:` group keyed on the tag
  (ci-image.yml's pattern), and this tool re-checks absence immediately
  before pushing: a tag another run published while this one built is
  kept, not overwritten.
* `reconcile-master` is the only writer of `master`. It runs in a
  separate job whose `concurrency:` group, MASTER_LOCK_GROUP, is ONE fixed
  group for every identity, so a run's decision and its write are one
  section that no other run's reconcile can interleave. (Per-identity
  groups do not serialize different identities, which let an older run's
  decision go stale before its write.)
* It points `master` at the identity of master's TIP, not of this run's
  commit: this run's own tag when the tip is this commit or a descendant
  with byte-identical plan inputs (PLAN_INPUT_PATHSPECS; covers docs-only
  and code-only commits), otherwise the tip's own plan, resolved from a
  worktree of the tip. So an older run never moves `master` back past a
  newer plan, an A -> B -> A rollback moves it back to A, and whichever
  reconcile executes last reconciles for the newest tip. That is why
  GitHub cancelling a PENDING reconcile when a later one queues (a group
  keeps one pending job) loses nothing. A tip whose identity is not
  published yet is left to that commit's run, whose reconcile queues only
  after it publishes. When master advances during the write, the target is
  decided again and repaired. A failed update recovers on a re-run or on
  the next push's reconcile.
* Every tag written is then fetched WITHOUT credentials: manifests,
  config and every layer blob. A package that is not public fails the job.

Consumption (`fetch`, `check`)
------------------------------
`fetch` installs the toolchain and store through the registry API, with no
Docker: it applies the image's layers in order, honouring whiteouts, but
writes only `usr/local/.ghcup` and `usr/local/cabal`, so the host's own
operating system is never touched. Every write and whiteout must resolve
inside one of those two prefixes as resolved under the root (not merely
inside the root, which is vacuous for the default `/`); symlinks in the
image are written as links but never followed, so a path through one, or a
prefix that is a symlink, makes it refuse the layer. Confinement is by
descriptor (`Jail`): every archive-supplied name is decoded to plain
components (never empty, `.`, `..` or containing a separator, after every
decoding step), and every create, remove, link and chmod is a `dir_fd` call
resolved component by component with `O_NOFOLLOW` from a prefix opened once,
so there is no path to re-resolve and no check-then-use gap. Everything
fetched is verified against the identity REQUESTED of it, never only the
registry's own header: a digest pin against the body's sha256, each child an
index selects against the digest and size the index declared, each blob
against its descriptor's digest and size, a `Docker-Content-Digest` header
must agree but is never the authority, and a tag is resolved once. A token
realm must be the registry's own origin, and a blob redirect an http(s) URL of
the registry's scheme. `check` compares a checkout's resolved
plan with the image's metadata by compiler, platform and non-local unit
IDs, which do not contain local paths, unlike plan.json's raw hash.

Standard library only: it runs on the 22.04 CI image's Python 3.10 (where
its tests run), on the 24.04 runner and in a consumer's sandbox.
"""
from __future__ import annotations

import argparse
import base64
import dataclasses
import datetime
import hashlib
import json
import os
import re
import shutil
import stat
import subprocess
import sys
import tarfile
import tempfile
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path, PurePosixPath
from typing import Callable, Iterable, Optional

REGISTRY = "ghcr.io"
REPOSITORY = "coghex/synarchy-deps"
MOVING_TAG = "master"
SOURCE_REPOSITORY = "coghex/synarchy"
PUBLISH_EVENT = "push"
PUBLISH_REF = "refs/heads/master"
DEFAULT_REF = f"{REGISTRY}/{REPOSITORY}:{MOVING_TAG}"

#: The producer's fixed resolution workspace and Cabal directory. plan.json
#: records absolute paths under both, so fixing them is what makes its hash
#: reproducible across runs.
WORKSPACE = "/src/synarchy"
CABAL_DIR = "/usr/local/cabal"
STORE_DIR = f"{CABAL_DIR}/store"
GHCUP_DIR = "/usr/local/.ghcup"
METADATA_PATH = f"{CABAL_DIR}/synarchy-deps.json"
METADATA_SCHEMA = 1
#: The only image paths `fetch` writes, relative to its root.
CONSUMER_PREFIXES = ("usr/local/.ghcup", "usr/local/cabal")
REQUIRED_PLATFORM = ("ubuntu", "24.04")

DOCKERFILE = ".github/deps/Dockerfile"
LABEL_PREFIX = "io.github.coghex.synarchy-deps"

MANIFEST_TYPES = (
    "application/vnd.docker.distribution.manifest.v2+json",
    "application/vnd.oci.image.manifest.v1+json",
)
INDEX_TYPES = (
    "application/vnd.docker.distribution.manifest.list.v2+json",
    "application/vnd.oci.image.index.v1+json",
)
ACCEPT = ", ".join(MANIFEST_TYPES + INDEX_TYPES)
#: The registry's "this does not exist" answers. Nothing else is absence.
ABSENT_CODES = frozenset({"MANIFEST_UNKNOWN", "NAME_UNKNOWN"})
PROFILING_SUFFIXES = (".p_hi", ".p_o", "_p.a")
TAG_PATTERN = re.compile(r"[A-Za-z0-9_][A-Za-z0-9._-]{0,127}")


class DepsImageError(Exception):
    """A failure this tool reports and exits non-zero on."""


class Refused(DepsImageError):
    """A write was asked for where none may happen (a pull request, say)."""


class RegistryError(DepsImageError):
    """The registry answered something other than the expected outcome."""


class NotAnonymous(DepsImageError):
    """Credential-free access to a manifest or blob was denied."""


# ---------------------------------------------------------------------------
# Identity

@dataclasses.dataclass(frozen=True)
class Identity:
    tag: str
    plan_sha256: str
    compiler_id: str
    compiler_abi: str
    os: str
    arch: str
    distro: str


def parse_os_release(text: str) -> dict[str, str]:
    values: dict[str, str] = {}
    for line in text.splitlines():
        if "=" in line and not line.lstrip().startswith("#"):
            key, _, value = line.partition("=")
            values[key.strip()] = value.strip().strip('"')
    return values


def _absolute_paths(value: object) -> Iterable[str]:
    if isinstance(value, str):
        if value.startswith("/"):
            yield value
    elif isinstance(value, dict):
        for item in value.values():
            yield from _absolute_paths(item)
    elif isinstance(value, list):
        for item in value:
            yield from _absolute_paths(item)


def plan_identity(plan_bytes: bytes, os_release_text: str) -> Identity:
    """The platform-marked publication identity of one resolved plan.

    Refuses a plan resolved anywhere but the fixed workspace and Cabal
    directory: its hash would then depend on where it ran, and the same
    plan would publish under a new tag on every run."""
    try:
        plan = json.loads(plan_bytes)
    except ValueError as error:
        raise DepsImageError(f"plan.json is not JSON: {error}") from error
    for key in ("compiler-id", "compiler-abi", "os", "arch", "install-plan"):
        if key not in plan:
            raise DepsImageError(f"plan.json has no {key!r}")
    stray = sorted({path for path in _absolute_paths(plan)
                    if not (path == WORKSPACE or path.startswith(WORKSPACE + "/")
                            or path.startswith(STORE_DIR + "/"))})
    if stray:
        raise DepsImageError(
            "plan.json was not resolved at the fixed workspace "
            f"{WORKSPACE} with CABAL_DIR={CABAL_DIR}; it records "
            f"{stray[0]!r}. Its hash would not be reproducible.")
    release = parse_os_release(os_release_text)
    distro_id, version = release.get("ID", ""), release.get("VERSION_ID", "")
    if not re.fullmatch(r"[a-z]+", distro_id) or not re.fullmatch(r"[0-9.]+", version):
        raise DepsImageError(f"unusable os-release ID={distro_id!r} VERSION_ID={version!r}")
    compiler = str(plan["compiler-id"])
    if not re.fullmatch(r"ghc-[0-9.]+", compiler):
        raise DepsImageError(f"unexpected compiler-id {compiler!r}")
    digest = hashlib.sha256(plan_bytes).hexdigest()
    distro = f"{distro_id}{version}"
    tag = f"{distro}-{plan['arch']}-{compiler.replace('-', '')}-{digest}"
    if not TAG_PATTERN.fullmatch(tag):
        raise DepsImageError(f"identity {tag!r} is not a valid tag")
    return Identity(tag=tag, plan_sha256=digest, compiler_id=compiler,
                    compiler_abi=str(plan["compiler-abi"]), os=str(plan["os"]),
                    arch=str(plan["arch"]), distro=distro)


def require_publishing_platform(identity: Identity) -> None:
    if identity.distro != "".join(REQUIRED_PLATFORM) or identity.os != "linux" \
            or identity.arch != "x86_64":
        raise DepsImageError(
            f"synarchy-deps is built on Ubuntu 24.04 x86_64 Linux; this plan "
            f"was resolved on {identity.distro} {identity.arch}-{identity.os}.")


def _unit_edges(unit: dict) -> list[str]:
    edges = list(unit.get("depends", [])) + list(unit.get("exe-depends", []))
    for component in (unit.get("components") or {}).values():
        edges += list(component.get("depends", [])) + list(component.get("exe-depends", []))
    return edges


def non_local_units(plan: dict) -> dict[str, list[str]]:
    """The dependency identities the checkout's own components need, which
    contain no local paths: store units (`configured`, style `global`) and
    the compiler's own packages (`pre-existing`).

    Only units reachable from the local packages' components (library,
    executables, both test suites, setup) count. plan.json also lists
    components nothing here builds -- a dependency's `readme` executable,
    another's benchmarks -- which neither `cabal build all` nor the test
    builds ever compile, so the store need not hold them."""
    units = {unit["id"]: unit for unit in plan.get("install-plan", [])}
    pending = [edge for unit in units.values() if unit.get("style") in ("local", "inplace")
               for edge in _unit_edges(unit)]
    reachable: set[str] = set()
    while pending:
        unit_id = pending.pop()
        if unit_id in reachable or unit_id not in units:
            continue
        reachable.add(unit_id)
        pending += _unit_edges(units[unit_id])
    store = sorted(i for i in reachable if units[i].get("style") == "global")
    compiler = sorted(i for i in reachable if units[i].get("type") == "pre-existing")
    return {"store": store, "compiler": compiler}


def local_packages(plan: dict) -> set[str]:
    return {f"{unit['pkg-name']}-{unit['pkg-version']}"
            for unit in plan.get("install-plan", [])
            if unit.get("style") in ("local", "inplace")}


def dry_run_builds(dry_run_text: str) -> list[str]:
    """`name-version` of every entry `cabal build --dry-run` would build."""
    entries, listing = [], False
    for line in dry_run_text.splitlines():
        if line.startswith("In order, the following "):
            listing = True
            continue
        match = re.match(r"^ - (\S+) ", line + " ") if listing else None
        if match:
            entries.append(match.group(1))
        elif listing and line.strip() and not line.startswith(" "):
            listing = False
    return entries


def dependency_builds(dry_run_text: str, plan: dict) -> list[str]:
    """Dry-run entries that are not the checkout's own packages."""
    local = local_packages(plan)
    return [entry for entry in dry_run_builds(dry_run_text) if entry not in local]


def compatibility(consumer_plan: dict, metadata: dict) -> list[str]:
    """Why the image's store cannot serve this plan in full; empty if it can.

    Compares the compiler, the platform and the non-local unit IDs, which
    hash each dependency's source, flags, dependencies and pkg-config
    versions but no local path, so it works from any checkout directory."""
    problems = []
    toolchain = metadata.get("toolchain", {})
    platform = metadata.get("platform", {})
    for label, mine, theirs in (
            ("compiler", consumer_plan.get("compiler-id"), toolchain.get("compiler-id")),
            ("compiler ABI", consumer_plan.get("compiler-abi"), toolchain.get("compiler-abi")),
            ("OS", consumer_plan.get("os"), platform.get("os")),
            ("architecture", consumer_plan.get("arch"), platform.get("arch"))):
        if mine != theirs:
            problems.append(f"{label}: checkout plan has {mine!r}, image has {theirs!r}")
    units = non_local_units(consumer_plan)
    image_units = metadata.get("units", {})
    for kind in ("store", "compiler"):
        missing = sorted(set(units[kind]) - set(image_units.get(kind, [])))
        for unit in missing:
            problems.append(f"not in the image ({kind}): {unit}")
    return problems


def store_metadata(plan_bytes: bytes, *, expect_plan_sha256: str,
                   dry_run_text: str, os_release_text: str,
                   source_revision: str, base_image: str, build_url: str,
                   cabal_version: str, store_dir: str = STORE_DIR,
                   index_state: str = "") -> dict:
    """Check a freshly built store against its plan and describe it.

    Fails unless the plan is the expected one, cabal's own dry-run would
    build nothing but the local packages, and every store unit the plan
    needs is present."""
    identity = plan_identity(plan_bytes, os_release_text)
    require_publishing_platform(identity)
    if identity.plan_sha256 != expect_plan_sha256:
        raise DepsImageError(
            f"this build resolved plan {identity.plan_sha256}, not the "
            f"{expect_plan_sha256} its tag was computed from; re-run the "
            "workflow so both jobs resolve the same plan.")
    plan = json.loads(plan_bytes)
    leftover = dependency_builds(dry_run_text, plan)
    if leftover:
        raise DepsImageError(
            "cabal would still build dependencies after the store build: "
            + ", ".join(leftover))
    units = non_local_units(plan)
    unit_root = Path(store_dir) / f"{identity.compiler_id}-{identity.compiler_abi}"
    missing = [unit for unit in units["store"] if not (unit_root / unit).is_dir()]
    if missing:
        raise DepsImageError(
            f"{len(missing)} store unit(s) of the plan are missing from "
            f"{unit_root}: {', '.join(missing[:5])}")
    release = parse_os_release(os_release_text)
    return {
        "schema": METADATA_SCHEMA,
        "identity": identity.tag,
        "plan_sha256": identity.plan_sha256,
        "producer": {
            "repository": SOURCE_REPOSITORY,
            "revision": source_revision,
            "build_url": build_url,
            "base_image": base_image,
            "workspace": WORKSPACE,
        },
        "toolchain": {
            "compiler-id": identity.compiler_id,
            "compiler-abi": identity.compiler_abi,
            "cabal-version": cabal_version,
            "ghcup_dir": GHCUP_DIR,
            "cabal_dir": CABAL_DIR,
            "store_dir": store_dir,
            "index_state": index_state,
        },
        "platform": {
            "os": identity.os,
            "arch": identity.arch,
            "distro": identity.distro,
            "pretty_name": release.get("PRETTY_NAME", ""),
        },
        "units": units,
    }


# ---------------------------------------------------------------------------
# Registry API client

@dataclasses.dataclass(frozen=True)
class Response:
    status: int
    headers: dict[str, str]
    body: bytes

    def header(self, name: str) -> str:
        for key, value in self.headers.items():
            if key.lower() == name.lower():
                return value
        return ""


#: Manifests, configs, token and error bodies are small JSON; a registry
#: that sends more than this is refused before it is parsed (the OCI
#: distribution spec asks registries to cap manifests at 4 MiB).
MAX_JSON_BODY = 4 << 20

DIGEST_PATTERN = re.compile(r"sha256:[0-9a-f]{64}")


def sha256_digest(body: bytes) -> str:
    return "sha256:" + hashlib.sha256(body).hexdigest()


def require_digest(value: object, what: str) -> str:
    """A well-formed sha256 digest, and nothing else: it is placed in URL
    paths and compared against computed digests, so a registry-supplied
    value is never used before it passes here."""
    if not isinstance(value, str) or not DIGEST_PATTERN.fullmatch(value):
        raise RegistryError(f"{what} is not a sha256 digest: {value!r}")
    return value


def descriptor(entry: object, what: str) -> tuple[str, int]:
    """The (digest, size) of an OCI descriptor. Both are required: the
    digest and size a manifest or index DECLARES are what the thing it
    points at must be verified against."""
    if not isinstance(entry, dict):
        raise RegistryError(f"{what} is not a descriptor")
    size = entry.get("size")
    if not isinstance(size, int) or isinstance(size, bool) or size < 0:
        raise RegistryError(f"{what} declares no valid size: {size!r}")
    return require_digest(entry.get("digest"), f"{what} digest"), size


class _NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, *args, **kwargs):  # noqa: D401
        return None


_OPENER = urllib.request.build_opener(_NoRedirect)


def http_request(method: str, url: str, headers: dict[str, str],
                 data: Optional[bytes] = None, timeout: float = 120.0,
                 sink: Optional[Callable[[bytes], None]] = None) -> Response:
    """One request, redirects NOT followed. With `sink`, a 2xx body is
    streamed to it instead of being returned."""
    request = urllib.request.Request(url, data=data, method=method, headers=headers)
    try:
        response = _OPENER.open(request, timeout=timeout)
    except urllib.error.HTTPError as error:
        return Response(error.code, dict(error.headers.items()), _bounded(error))
    except (urllib.error.URLError, OSError) as error:
        raise RegistryError(f"{method} {url}: {error}") from error
    with response:
        if sink is not None and 200 <= response.status < 300:
            while True:
                chunk = response.read(1 << 20)
                if not chunk:
                    break
                sink(chunk)
            return Response(response.status, dict(response.headers.items()), b"")
        return Response(response.status, dict(response.headers.items()), _bounded(response))


def _bounded(response) -> bytes:
    body = response.read(MAX_JSON_BODY + 1)
    if len(body) > MAX_JSON_BODY:
        raise RegistryError(f"response body exceeds {MAX_JSON_BODY} bytes; refused")
    return body


def error_codes(body: bytes) -> set[str]:
    try:
        errors = json.loads(body).get("errors") or []
    except (ValueError, AttributeError):
        return set()
    return {str(item.get("code")) for item in errors if isinstance(item, dict)}


@dataclasses.dataclass(frozen=True)
class ManifestState:
    present: bool
    digest: str = ""
    media_type: str = ""
    body: bytes = b""


class Registry:
    """The distribution API subset this tool needs, with the bearer-token
    challenge flow ghcr.io uses. `credentials=None` is an anonymous client."""

    def __init__(self, base_url: str = f"https://{REGISTRY}",
                 repository: str = REPOSITORY,
                 credentials: Optional[tuple[str, str]] = None,
                 transport: Callable[..., Response] = http_request):
        self.base_url = base_url.rstrip("/")
        self.repository = repository
        self.credentials = credentials
        self.transport = transport
        self._token: Optional[str] = None

    def _url(self, path: str) -> str:
        return f"{self.base_url}/v2/{self.repository}/{path}"

    @staticmethod
    def _origin(url: str) -> tuple[str, str, int]:
        parts = urllib.parse.urlsplit(url)
        default = {"https": 443, "http": 80}.get(parts.scheme, 0)
        return parts.scheme, (parts.hostname or "").lower(), parts.port or default

    def _fetch_token(self, challenge: str) -> None:
        params = dict(re.findall(r'(\w+)="([^"]*)"', challenge))
        realm = params.pop("realm", "")
        if not challenge.lower().startswith("bearer") or not realm:
            raise RegistryError(f"unsupported auth challenge {challenge!r}")
        # The realm is registry-supplied, and the request to it carries the
        # push credentials: it must be the registry's own origin.
        if self._origin(realm) != self._origin(self.base_url):
            raise RegistryError(f"token realm {realm!r} is not the registry's own origin; "
                                "credentials are not sent elsewhere")
        if self.credentials is not None:
            params["scope"] = f"repository:{self.repository}:pull,push"
        headers = {}
        if self.credentials is not None:
            user, secret = self.credentials
            headers["Authorization"] = "Basic " + base64.b64encode(
                f"{user}:{secret}".encode()).decode()
        response = self.transport("GET", realm + "?" + urllib.parse.urlencode(params), headers)
        if response.status in (401, 403):
            detail = (f"token for {self.repository} ({response.status} "
                      f"{', '.join(sorted(error_codes(response.body))) or 'no error code'})")
            if self.credentials is not None:
                raise RegistryError(f"the registry refused a push {detail}; does the "
                                    "job have `packages: write`?")
            raise NotAnonymous(f"the registry refused an anonymous pull {detail}")
        if response.status != 200:
            raise RegistryError(f"token request answered {response.status}")
        try:
            payload = json.loads(response.body)
        except ValueError as error:
            raise RegistryError("token response is not JSON") from error
        token = payload.get("token") or payload.get("access_token")
        if not token:
            raise RegistryError("token response carries no token")
        self._token = token

    def request(self, method: str, path: str, headers: Optional[dict[str, str]] = None,
                data: Optional[bytes] = None,
                sink: Optional[Callable[[bytes], None]] = None) -> Response:
        url = path if path.startswith(("http://", "https://")) else self._url(path)
        for attempt in (1, 2):
            send = dict(headers or {})
            if self._token:
                send["Authorization"] = f"Bearer {self._token}"
            response = self.transport(method, url, send, data=data, sink=sink)
            challenge = response.header("WWW-Authenticate")
            if response.status == 401 and challenge and attempt == 1:
                self._fetch_token(challenge)
                continue
            return response
        return response

    def manifest(self, reference: str, expect_size: Optional[int] = None) -> ManifestState:
        """Confirmed presence or confirmed absence; anything else raises.

        Integrity is against the REQUESTED identity, never the registry's
        own claim: a digest reference (`sha256:...`) must be exactly what the
        body hashes to, a declared `expect_size` must be its length, and a
        `Docker-Content-Digest` header, where present, must agree with the
        body too but is never the authority. A tag has no identity to check
        against: the caller pins the digest it resolved to and verifies
        everything after it against that."""
        if ":" in reference:
            require_digest(reference, "manifest reference")
        response = self.request("GET", f"manifests/{reference}", {"Accept": ACCEPT})
        if response.status == 200:
            digest = sha256_digest(response.body)
            if ":" in reference and digest != reference:
                raise RegistryError(
                    f"manifest {reference}: the registry returned a body that hashes "
                    f"to {digest}")
            if expect_size is not None and len(response.body) != expect_size:
                raise RegistryError(
                    f"manifest {reference} is {len(response.body)} bytes, but "
                    f"{expect_size} were declared")
            claimed = response.header("Docker-Content-Digest")
            if claimed and claimed != digest:
                raise RegistryError(
                    f"manifest {reference} digest header {claimed} does not "
                    f"match its body {digest}")
            media = response.header("Content-Type").split(";")[0].strip()
            return ManifestState(True, digest, media, response.body)
        codes = error_codes(response.body)
        if response.status == 404 and codes & ABSENT_CODES:
            return ManifestState(False)
        if response.status in (401, 403):
            raise NotAnonymous(
                f"{self.repository}:{reference} denied ({response.status} "
                f"{', '.join(sorted(codes)) or 'no error code'})")
        raise RegistryError(
            f"could not determine whether {self.repository}:{reference} exists: "
            f"HTTP {response.status} {', '.join(sorted(codes)) or 'no error code'}. "
            "Only a 404 MANIFEST_UNKNOWN/NAME_UNKNOWN is a confirmed absence; "
            "failing closed rather than risking an overwrite.")

    def put_manifest(self, reference: str, body: bytes, media_type: str) -> str:
        response = self.request("PUT", f"manifests/{reference}",
                                {"Content-Type": media_type}, data=body)
        if response.status not in (200, 201):
            raise RegistryError(
                f"PUT manifest {reference} answered {response.status} "
                f"{', '.join(sorted(error_codes(response.body)))}")
        digest = sha256_digest(body)
        claimed = response.header("Docker-Content-Digest")
        if claimed and claimed != digest:
            raise RegistryError(f"PUT manifest {reference}: the registry reports "
                                f"{claimed}, but the body is {digest}")
        return digest

    def blob_accessible(self, digest: str) -> bool:
        """HEAD a blob without following ghcr's redirect to its CDN: a
        redirect or 200 means the registry grants access; 401/403 does not."""
        require_digest(digest, "blob")
        response = self.request("HEAD", f"blobs/{digest}")
        if response.status in (200, 301, 302, 303, 307, 308):
            return True
        if response.status in (401, 403):
            return False
        raise RegistryError(f"HEAD blob {digest} answered {response.status}")

    def download_blob(self, digest: str, sink: Callable[[bytes], None],
                      size: Optional[int] = None) -> None:
        """Stream a blob to `sink`, following one redirect WITHOUT the
        registry's Authorization header (the CDN URL is pre-signed), and
        verify what arrived against the REQUESTED digest and, when the
        descriptor declared one, its size. A stream longer than the declared
        size is cut off as soon as it exceeds it. The sink has seen the bytes
        by the time this returns, so a caller uses them only after it does."""
        require_digest(digest, "blob")
        hasher = hashlib.sha256()
        received = 0

        def both(chunk: bytes) -> None:
            nonlocal received
            received += len(chunk)
            if size is not None and received > size:
                raise RegistryError(f"blob {digest} is longer than its declared {size} bytes")
            hasher.update(chunk)
            sink(chunk)

        response = self.request("GET", f"blobs/{digest}", sink=both)
        if response.status in (301, 302, 303, 307, 308):
            location = urllib.parse.urljoin(self._url(f"blobs/{digest}"),
                                            response.header("Location"))
            if urllib.parse.urlsplit(location).scheme != self._origin(self.base_url)[0]:
                raise RegistryError(f"blob {digest} redirects to {location!r}, which is not "
                                    f"a {self._origin(self.base_url)[0]} URL; refused")
            response = self.transport("GET", location, {}, sink=both)
        if response.status != 200:
            raise RegistryError(f"GET blob {digest} answered {response.status}")
        if size is not None and received != size:
            raise RegistryError(f"blob {digest} is {received} bytes, but {size} were declared")
        if "sha256:" + hasher.hexdigest() != digest:
            raise RegistryError(f"blob {digest} failed its digest check")

    def blob_bytes(self, digest: str, size: Optional[int] = None) -> bytes:
        chunks: list[bytes] = []
        self.download_blob(digest, chunks.append, size)
        return b"".join(chunks)


@dataclasses.dataclass(frozen=True)
class ResolvedImage:
    """What a reference resolved to, each step verified: `top` is the
    manifest the reference named (an index, for a multi-platform image),
    `image` the linux/amd64 image manifest, `document` its parsed body."""
    top: ManifestState
    image: ManifestState
    document: dict


#: An index may name an index; no real image nests deeper, and a hostile one
#: must not recurse without bound.
MAX_INDEX_DEPTH = 3


def resolve_image(registry: Registry, reference: str, _depth: int = 0,
                  _expect: Optional[tuple[str, int]] = None) -> ResolvedImage:
    """The linux/amd64 image manifest behind a tag or digest.

    Every hop is verified against the identity that was REQUESTED of it: a
    digest reference against its own body, and each child an index selects
    against the digest AND size the index declared. A tag is resolved once
    and the digest it resolved to is what the rest is verified against."""
    if _depth > MAX_INDEX_DEPTH:
        raise DepsImageError(f"{reference}: image index nests too deeply")
    state = registry.manifest(_expect[0] if _expect else reference,
                              expect_size=_expect[1] if _expect else None)
    if not state.present:
        raise DepsImageError(f"{registry.repository}:{reference} does not exist")
    try:
        document = json.loads(state.body)
    except ValueError as error:
        raise RegistryError(f"manifest {reference} is not JSON") from error
    if not isinstance(document, dict):
        raise RegistryError(f"manifest {reference} is not a JSON object")
    media = document.get("mediaType") or state.media_type
    if media in INDEX_TYPES:
        for entry in document.get("manifests", []):
            platform = entry.get("platform", {}) if isinstance(entry, dict) else {}
            if platform.get("os") == "linux" and platform.get("architecture") == "amd64":
                child = resolve_image(registry, reference, _depth + 1,
                                      _expect=descriptor(entry, f"{reference}: index entry"))
                return ResolvedImage(state if _depth == 0 else child.top, child.image,
                                     child.document)
        raise DepsImageError(f"{reference} has no linux/amd64 image")
    if media not in MANIFEST_TYPES:
        raise DepsImageError(f"{reference} has unsupported media type {media!r}")
    image_blobs(document, reference)           # malformed descriptors fail here
    return ResolvedImage(state, state, document)


def image_blobs(document: dict, reference: str = "image"
                ) -> tuple[tuple[str, int], list[tuple[str, int]]]:
    """The (digest, size) of an image manifest's config and each layer,
    every one required and well-formed."""
    config = descriptor(document.get("config"), f"{reference}: config")
    layers = [descriptor(layer, f"{reference}: layer {index}")
              for index, layer in enumerate(document.get("layers") or [], 1)]
    return config, layers


def image_manifest(registry: Registry, reference: str) -> tuple[ManifestState, dict]:
    """The linux/amd64 image manifest behind a tag or digest."""
    resolved = resolve_image(registry, reference)
    return resolved.image, resolved.document


def verify_anonymous(registry: Registry, tags: Iterable[str]) -> list[str]:
    """Fetch each tag's manifest and HEAD its config and every layer with
    an anonymous client. Raises NotAnonymous with the remedy."""
    report = []
    for tag in tags:
        try:
            resolved = resolve_image(registry, tag)
            state = resolved.image
            config, layers = image_blobs(resolved.document, tag)
            blobs = [config[0]] + [digest for digest, _size in layers]
            denied = [blob for blob in blobs if not registry.blob_accessible(blob)]
        except NotAnonymous as error:
            raise NotAnonymous(
                f"{error}. The package must be public: on "
                f"https://github.com/users/{REPOSITORY.split('/')[0]}/packages/"
                f"container/{REPOSITORY.split('/')[1]}/settings choose "
                "'Change visibility' -> Public.") from error
        if denied:
            raise NotAnonymous(
                f"{tag}: {len(denied)} blob(s) refuse anonymous access, "
                f"first {denied[0]}")
        top = resolved.top.digest       # the digest this tag resolved to, not a second fetch
        named = top if top == state.digest else f"{top} (linux/amd64 {state.digest})"
        report.append(f"{tag} -> {named}: manifests and {len(blobs)} "
                      "blob(s) readable without credentials")
    return report


# ---------------------------------------------------------------------------
# Publication

def publication_refusal(event: str, ref: str, repository: str) -> Optional[str]:
    """Why this run may not write to the registry, or None if it may."""
    if event != PUBLISH_EVENT:
        return (f"event {event!r}: only pushes to master publish "
                "synarchy-deps; pull requests never do, same-repository "
                "ones included")
    if ref != PUBLISH_REF:
        return f"ref {ref!r}: only {PUBLISH_REF} publishes"
    if repository != SOURCE_REPOSITORY:
        return f"repository {repository!r}: only {SOURCE_REPOSITORY} publishes"
    return None


#: What the identity depends on, as git pathspecs (top-level `cabal.project*`
#: and `*.cabal`, and the image recipe): plan.json is resolved from exactly
#: these files inside the recipe's toolchain stage. Two commits whose plan
#: inputs are byte-identical resolve the same identity, the same assumption
#: the docs-only fast path makes about a documentation change.
PLAN_INPUT_PATHSPECS = (":(glob)cabal.project*", ":(glob)*.cabal", DOCKERFILE)

#: The workflow's one cross-identity lock (deps-image.yml's `deps-master`
#: job). Every `master` decision and write happens under it.
MASTER_LOCK_GROUP = "synarchy-deps-master"


@dataclasses.dataclass(frozen=True)
class MasterTarget:
    """Which identity `master` should name for master's tip, and how that
    was established: `own` (this run's identity covers the tip) or `tip`
    (the tip's plan was resolved, so its identity must be looked up)."""
    source: str
    reason: str


def master_target(own: str, tip: str, own_is_ancestor: bool,
                  inputs_changed: Optional[bool]) -> MasterTarget:
    """How to find the identity of master's tip from this run's commit.

    This run's identity covers the tip when its commit is the tip, or an
    ancestor of it with byte-identical plan inputs (later commits changed
    only code, docs or anything else the plan is not resolved from).
    Otherwise the tip's own plan must be resolved: deciding by this run's
    commit alone would let an older run move `master` back past a newer
    plan, and leaving `master` alone could lose the newer run's update if
    GitHub cancelled its pending reconcile in the shared lock."""
    if own == tip:
        return MasterTarget("own", "this run's commit is master's tip")
    if own_is_ancestor and inputs_changed is False:
        return MasterTarget("own", f"no plan input changed between {own[:12]} "
                                   f"and master's tip {tip[:12]}")
    if not own_is_ancestor:
        return MasterTarget("tip", f"{own[:12]} is not an ancestor of master's tip "
                                   f"{tip[:12]}; resolving the tip's plan")
    return MasterTarget("tip", f"plan inputs changed between {own[:12]} and "
                               f"master's tip {tip[:12]}; resolving the tip's plan")


class Git:
    def __init__(self, repo: str = "."):
        self.repo = repo

    def _git(self, *args: str) -> subprocess.CompletedProcess:
        return subprocess.run(["git", "-C", self.repo, *args],
                              capture_output=True, text=True)

    def master_tip(self) -> str:
        fetched = self._git("fetch", "--no-tags", "origin",
                            "+refs/heads/master:refs/remotes/origin/master")
        if fetched.returncode != 0:
            raise DepsImageError(f"git fetch origin master failed: {fetched.stderr.strip()}")
        return self._git("rev-parse", "refs/remotes/origin/master").stdout.strip()

    def is_ancestor(self, older: str, newer: str) -> bool:
        return self._git("merge-base", "--is-ancestor", older, newer).returncode == 0

    def plan_inputs_changed(self, older: str, newer: str) -> bool:
        """Whether any plan input differs between two commits. An error
        counts as a change, which only costs a resolution of the tip."""
        diff = self._git("diff", "--name-only", older, newer, "--", *PLAN_INPUT_PATHSPECS)
        return diff.returncode != 0 or bool(diff.stdout.strip())

    def worktree(self, commit: str, path: str) -> None:
        added = self._git("worktree", "add", "--detach", path, commit)
        if added.returncode != 0:
            raise DepsImageError(f"git worktree add {commit[:12]} failed: "
                                 f"{added.stderr.strip()}")

    def remove_worktree(self, path: str) -> None:
        self._git("worktree", "remove", "--force", path)


def decide_master_target(git: Git, own: str) -> tuple[str, MasterTarget]:
    """Master's tip now, and how to find the identity it should name."""
    tip = git.master_tip()
    if tip == own:
        return tip, master_target(own, tip, True, None)
    ancestor = git.is_ancestor(own, tip)
    return tip, master_target(own, tip, ancestor,
                              git.plan_inputs_changed(own, tip) if ancestor else None)


class Docker:
    """The docker CLI calls the publisher makes; tests substitute a fake."""

    def __init__(self, context: str = ".", run: Callable[..., subprocess.CompletedProcess] = subprocess.run):
        self.context = context
        self._run = run

    def _docker(self, *args: str, capture: bool = False) -> str:
        result = self._run(["docker", *args], text=True,
                           capture_output=capture, check=False)
        if result.returncode != 0:
            raise DepsImageError(f"docker {args[0]} failed ({result.returncode})"
                                 + (f": {result.stderr.strip()}" if capture else ""))
        return result.stdout if capture else ""

    def resolve_base(self, reference: str, pull: bool = True) -> str:
        """An immutable `name@sha256:...` for the base image."""
        if "@sha256:" in reference:
            return reference
        if pull:
            self._docker("pull", "--quiet", reference)
        digests = json.loads(self._docker(
            "image", "inspect", "--format", "{{json .RepoDigests}}", reference,
            capture=True) or "[]") or []
        if digests:
            return digests[0]
        image_id = self._docker("image", "inspect", "--format", "{{.Id}}",
                                reference, capture=True).strip()
        if pull or not image_id:
            raise DepsImageError(f"{reference} has no registry digest")
        return f"{reference}@{image_id}"

    def build(self, target: str, build_args: dict[str, str],
              labels: Optional[dict[str, str]] = None,
              tag: Optional[str] = None, output: Optional[str] = None) -> None:
        args = ["build", "--file", os.path.join(self.context, DOCKERFILE),
                "--target", target]
        for key, value in build_args.items():
            args += ["--build-arg", f"{key}={value}"]
        for key, value in (labels or {}).items():
            args += ["--label", f"{key}={value}"]
        if tag:
            args += ["--tag", tag]
        if output:
            args += ["--output", f"type=local,dest={output}"]
        self._docker(*args, self.context)

    def verify(self, image: str, tag: str, checkout: str) -> None:
        self._docker("run", "--rm", "--network", "none",
                     "--volume", f"{os.path.abspath(checkout)}:/src/checkout:ro",
                     image, "python3", "/src/checkout/tools/ci_deps_image.py",
                     "verify-image", "--checkout", "/src/checkout",
                     "--expect-tag", tag)

    def push(self, local: str, remote: str) -> None:
        self._docker("tag", local, remote)
        self._docker("push", "--quiet", remote)


def registry_host(registry: Registry) -> str:
    """The `docker push` host for the registry this run checked, so the
    push and every check address the same registry."""
    return urllib.parse.urlparse(registry.base_url).netloc


def build_args(base_image: str, plan_sha256: str = "", revision: str = "",
               build_url: str = "") -> dict[str, str]:
    values = {"BASE_IMAGE": base_image}
    if plan_sha256:
        values.update(EXPECTED_PLAN_SHA256=plan_sha256, SOURCE_REVISION=revision,
                      BUILD_URL=build_url)
    return values


def image_labels(identity_tag: str, plan_sha256: str, revision: str,
                 base_image: str, build_url: str, created: str) -> dict[str, str]:
    name, _, digest = base_image.partition("@")
    return {
        "org.opencontainers.image.title": "synarchy-deps",
        "org.opencontainers.image.description":
            "Ubuntu 24.04 + GHC/cabal + the compiled Cabal store for one "
            "synarchy dependency plan (docs/deps_image.md)",
        "org.opencontainers.image.source": f"https://github.com/{SOURCE_REPOSITORY}",
        "org.opencontainers.image.revision": revision,
        "org.opencontainers.image.created": created,
        "org.opencontainers.image.base.name": name,
        "org.opencontainers.image.base.digest": digest,
        f"{LABEL_PREFIX}.identity": identity_tag,
        f"{LABEL_PREFIX}.plan-sha256": plan_sha256,
        f"{LABEL_PREFIX}.build-url": build_url,
        f"{LABEL_PREFIX}.metadata": METADATA_PATH,
    }


@dataclasses.dataclass
class PublishResult:
    tag: str
    digest: str
    built: bool
    pushed: bool
    anonymous: list[str]
    notes: list[str]


def publish(*, registry: Registry, anonymous: Registry, docker: Docker,
            event: str, ref: str, repository: str, tag: str, plan_sha256: str,
            base_image: str, revision: str, build_url: str, checkout: str = ".",
            now: Callable[[], str] = lambda: datetime.datetime.now(
                datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
            log: Callable[[str], None] = print) -> PublishResult:
    """Push this run's immutable identity tag at most once. `master` is not
    touched here: `reconcile_master` moves it, under the cross-identity
    lock, after this succeeds."""
    refusal = publication_refusal(event, ref, repository)
    if refusal:
        raise Refused(refusal)
    if not TAG_PATTERN.fullmatch(tag) or not tag.endswith(plan_sha256) or not plan_sha256:
        raise DepsImageError(f"tag {tag!r} does not carry plan hash {plan_sha256!r}")
    notes: list[str] = []
    built = pushed = False
    state = registry.manifest(tag)
    if state.present:
        log(f"{tag} is already published ({state.digest}); write-once: not rebuilding it")
    else:
        log(f"{tag} is not published yet (confirmed absent); building it")
        local = f"synarchy-deps:{tag}"
        docker.build("image", build_args(base_image, plan_sha256, revision, build_url),
                     labels=image_labels(tag, plan_sha256, revision, base_image,
                                         build_url, now()),
                     tag=local)
        built = True
        docker.verify(local, tag, checkout)
        # Re-checked right before the push: a run of the same identity that
        # finished first keeps its image (and provenance).
        again = registry.manifest(tag)
        if again.present:
            notes.append(f"another run published {tag} while this one built; "
                         "kept its image, did not overwrite it")
            log(notes[-1])
        else:
            docker.push(local, f"{registry_host(registry)}/{registry.repository}:{tag}")
            pushed = True
        state = registry.manifest(tag)
        if not state.present:
            raise RegistryError(f"{tag} is still absent after the push")
    report = verify_anonymous(anonymous, [tag])
    for line in report:
        log(line)
    return PublishResult(tag, state.digest, built, pushed, report, notes)


@dataclasses.dataclass
class ReconcileResult:
    tip: str
    target: str
    digest: str
    moved: bool
    reason: str
    anonymous: list[str]
    notes: list[str]


#: Re-decisions after master advanced during one reconcile. Each later push
#: has its own reconcile queued behind the lock, so stopping is safe.
RECONCILE_ROUNDS = 3


def reconcile_master(*, registry: Registry, anonymous: Registry, git: Git,
                     resolve_tip: Callable[[str], str], event: str, ref: str,
                     repository: str, tag: str, revision: str,
                     rounds: int = RECONCILE_ROUNDS,
                     log: Callable[[str], None] = print) -> ReconcileResult:
    """Point `master` at the identity of master's tip. Runs under the
    workflow's cross-identity lock (MASTER_LOCK_GROUP), so the decision and
    the write below are one section no other run's reconcile interleaves.

    The target is the tip's identity, not this run's: this run's own tag
    when it covers the tip (`master_target`), otherwise the tip's plan
    resolved by `resolve_tip`. So an older run never moves `master` back
    past a newer plan, and whichever run's reconcile executes last
    reconciles for the newest tip, which is what makes it safe for GitHub
    to cancel a pending reconcile when a later one queues. A tip whose
    identity is not published yet is left to the tip's own run, whose
    reconcile queues only after it publishes. After writing, master's tip
    is read again; if it advanced, the target is decided again (at most
    `rounds` times), so a write made just before a push is repaired here
    rather than left for the next run."""
    refusal = publication_refusal(event, ref, repository)
    if refusal:
        raise Refused(refusal)
    own = registry.manifest(tag)
    if not own.present:
        raise DepsImageError(f"{tag} is not published; the publish job must "
                             "succeed before master is reconciled")
    notes: list[str] = []
    moved = False
    target_tag, target_state, reason, tip = tag, own, "", revision
    for round_ in range(1, rounds + 1):
        tip, how = decide_master_target(git, revision)
        target_tag = tag if how.source == "own" else resolve_tip(tip)
        reason = how.reason
        log(f"{MOVING_TAG}: target for master's tip {tip[:12]} is {target_tag} ({reason})")
        target_state = own if target_tag == tag else registry.manifest(target_tag)
        if not target_state.present:
            reason = (f"master's tip {tip[:12]} resolves to {target_tag}, which is "
                      "not published yet; that commit's run publishes it and "
                      "reconciles master after this")
            log(f"{MOVING_TAG}: left alone ({reason})")
            break
        current = registry.manifest(MOVING_TAG)
        if current.present and current.digest == target_state.digest:
            log(f"{MOVING_TAG} already names {target_state.digest}")
        else:
            registry.put_manifest(MOVING_TAG, target_state.body, target_state.media_type)
            check = registry.manifest(MOVING_TAG)
            if check.digest != target_state.digest:
                raise RegistryError(f"{MOVING_TAG} reads back {check.digest}, "
                                    f"not {target_state.digest}")
            moved = True
            log(f"{MOVING_TAG} -> {target_tag} ({target_state.digest})")
        after = git.master_tip()
        if after == tip:
            break
        notes.append(f"master advanced from {tip[:12]} to {after[:12]} during "
                     "the update; deciding again")
        log(notes[-1])
    else:
        notes.append(f"master kept advancing for {rounds} rounds; the newer "
                     "runs' reconciles, queued behind this lock, continue it")
        log(notes[-1])
    current_master = registry.manifest(MOVING_TAG)
    report = verify_anonymous(anonymous, [MOVING_TAG]) if current_master.present else []
    for line in report:
        log(line)
    return ReconcileResult(tip, target_tag, target_state.digest if target_state.present else "",
                           moved, reason, report, notes)


def resolve_tip_identity(git: Git, docker_factory: Callable[[str], "Docker"],
                         base_image: str, commit: str) -> str:
    """The identity tag of `commit`'s own dependency plan, resolved exactly
    as the plan job resolves it, from a temporary worktree of that commit."""
    with tempfile.TemporaryDirectory(prefix="synarchy-deps-tip-") as scratch:
        tree = os.path.join(scratch, "tree")
        git.worktree(commit, tree)
        try:
            out = os.path.join(scratch, "plan")
            docker_factory(tree).build("plan-export", build_args(base_image), output=out)
            identity = plan_identity(Path(out, "plan.json").read_bytes(),
                                     Path(out, "os-release").read_text())
        finally:
            git.remove_worktree(tree)
    require_publishing_platform(identity)
    return identity.tag


# ---------------------------------------------------------------------------
# Docker-free consumption

def _component(name: str, what: str) -> str:
    """One decoded path component: never empty, `.` or `..`, and never
    containing a separator or NUL. Every name the archive supplies is
    decoded into components, and every component passes here, whatever
    produced it (an entry name, a whiteout's victim, a link target): a
    decoding step such as stripping `.wh.` can turn a harmless-looking name
    into `..`."""
    if name in ("", ".", "..") or "/" in name or "\0" in name:
        raise DepsImageError(f"{what}: component {name!r} is not a plain name")
    return name


def _member_parts(name: str, what: str = "layer entry") -> tuple[str, ...]:
    """An archive member name as a tuple of plain components. A name is
    relative: absolute paths, empty or `.` or `..` components anywhere are
    refused; only a leading `./` and a trailing `/` (a directory) are
    normalised away. `./` alone is the archive root: no components."""
    if "\0" in name or name.startswith("/"):
        raise DepsImageError(f"{what} {name!r} is absolute or malformed")
    parts = name.split("/")
    if parts and parts[-1] == "":
        parts.pop()
    if parts and parts[0] == ".":
        parts = parts[1:]
    return tuple(_component(part, f"{what} {name!r}") for part in parts)


def _prefix_parts(prefixes: tuple[str, ...]) -> dict[str, tuple[str, ...]]:
    return {prefix: tuple(PurePosixPath(prefix).parts) for prefix in prefixes}


def _prefix_of(parts: tuple[str, ...], by_prefix: dict[str, tuple[str, ...]]) -> Optional[str]:
    """The prefix `parts` is, or is below."""
    for prefix, prefix_parts in by_prefix.items():
        if parts[:len(prefix_parts)] == prefix_parts:
            return prefix
    return None


def _ancestor_prefixes(parts: tuple[str, ...],
                       by_prefix: dict[str, tuple[str, ...]]) -> list[str]:
    """The prefixes that lie strictly below `parts`."""
    return [prefix for prefix, prefix_parts in by_prefix.items()
            if len(parts) < len(prefix_parts) and prefix_parts[:len(parts)] == parts]


_OPEN_DIR = os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW | getattr(os, "O_CLOEXEC", 0)
_OPEN_NEW = (os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW
             | getattr(os, "O_CLOEXEC", 0))


def _open_dir(parent_fd: int, name: str, what: str) -> int:
    """Open the directory `name` of `parent_fd`, never through a symlink."""
    try:
        return os.open(name, _OPEN_DIR, dir_fd=parent_fd)
    except FileNotFoundError:
        raise
    except OSError as error:
        try:
            symlink = stat.S_ISLNK(os.stat(name, dir_fd=parent_fd, follow_symlinks=False).st_mode)
        except OSError:
            symlink = False
        if symlink:
            raise DepsImageError(f"{what} passes through the symlink {name!r}; "
                                 "symlinks are never followed") from error
        raise DepsImageError(f"{what}: {name!r} is not a directory") from error


def _remove_at(parent_fd: int, name: str) -> None:
    """Remove `name` from `parent_fd`: a symlink or file is unlinked, a
    directory is emptied and removed, all by descriptor-relative calls that
    never follow a link. Absent is fine."""
    try:
        mode = os.stat(name, dir_fd=parent_fd, follow_symlinks=False).st_mode
    except FileNotFoundError:
        return
    if stat.S_ISDIR(mode):
        fd = _open_dir(parent_fd, name, "removal")
        try:
            for child in os.listdir(fd):
                _remove_at(fd, child)
        finally:
            os.close(fd)
        os.rmdir(name, dir_fd=parent_fd)
    else:
        os.unlink(name, dir_fd=parent_fd)


def is_within(real_root: str, real_path: str) -> bool:
    """Whether a resolved path is the resolved root or below it (the root
    may be `/` itself, the documented default)."""
    return os.path.commonpath([real_root, real_path]) == real_root


def resolved_prefixes(root: Path, prefixes: tuple[str, ...] = CONSUMER_PREFIXES
                      ) -> dict[str, str]:
    """Where each prefix really is under `root`, resolved before anything
    is written. Directories above a prefix (`/usr/local`, say) belong to the
    host and are taken as the host configured them; the prefix itself must
    not be a symlink, because a prefix that redirects elsewhere would carry
    every entry with it."""
    real_root = os.path.realpath(root)
    resolved = {}
    for prefix in prefixes:
        host = root.joinpath(*PurePosixPath(prefix).parts)
        if host.is_symlink():
            raise DepsImageError(f"{host} is a symlink; a prefix is never followed. "
                                 "Start from empty, real prefix directories.")
        real = os.path.realpath(host)
        if not is_within(real_root, real):
            raise DepsImageError(f"{host} resolves to {real}, outside {real_root}")
        resolved[prefix] = real
    return resolved


def within_prefixes(real_prefixes: Iterable[str], real_path: str) -> bool:
    """Whether a resolved path is one of the resolved prefixes or below one.
    This, not containment in the root, is the confinement rule: with the
    default root `/`, every absolute path is within the root."""
    return any(is_within(prefix, real_path) for prefix in real_prefixes)


class Jail:
    """Where one layer's entries may land: inside the prefixes, through real
    directories only, by descriptor-relative operations.

    Each prefix is opened once, with `O_NOFOLLOW`, from its host parent (the
    directories above a prefix, `/usr/local` say, belong to the host and are
    taken as the host configured them; the prefix itself must not be a
    symlink). Everything the archive names is then resolved component by
    component FROM that descriptor with `O_NOFOLLOW`, and every create,
    remove, link and chmod is a `dir_fd` call on the parent so reached. There
    is no path string to re-resolve, so no check-then-use gap: a component
    swapped for a symlink after a check is refused when it is opened, and no
    operation can address anything but the entry it names inside the prefix
    tree held by the descriptor. Symlinks in the image are written as links
    and never followed."""

    def __init__(self, root: Path, prefixes: tuple[str, ...] = CONSUMER_PREFIXES):
        self.root = root
        self.prefixes = tuple(prefixes)
        self.by_prefix = _prefix_parts(self.prefixes)
        # raises for a prefix that is a symlink or resolves outside the root
        self.real = resolved_prefixes(root, self.prefixes)
        self._fds: dict[str, int] = {}

    def close(self) -> None:
        for fd in self._fds.values():
            os.close(fd)
        self._fds.clear()

    def __enter__(self) -> "Jail":
        return self

    def __exit__(self, *exc: object) -> None:
        self.close()

    # -- the prefixes ------------------------------------------------------
    def _host_parent(self, prefix: str, create: bool) -> Optional[int]:
        parent = self.root.joinpath(*self.by_prefix[prefix][:-1])
        if create:
            parent.mkdir(parents=True, exist_ok=True)
        real_parent = os.path.realpath(parent)
        if not is_within(os.path.realpath(self.root), real_parent):
            raise DepsImageError(f"{parent} resolves to {real_parent}, outside {self.root}")
        try:
            return os.open(parent, os.O_RDONLY | os.O_DIRECTORY | getattr(os, "O_CLOEXEC", 0))
        except FileNotFoundError:
            return None

    def prefix_fd(self, prefix: str, create: bool) -> Optional[int]:
        """The descriptor of a prefix root (shared: do not close), or
        `None` when it does not exist and `create` is false."""
        if prefix in self._fds:
            return self._fds[prefix]
        parent = self._host_parent(prefix, create)
        if parent is None:
            return None
        name = self.by_prefix[prefix][-1]
        try:
            try:
                fd = _open_dir(parent, name, f"prefix {prefix}")
            except FileNotFoundError:
                if not create:
                    return None
                os.mkdir(name, 0o777, dir_fd=parent)
                fd = _open_dir(parent, name, f"prefix {prefix}")
        except DepsImageError as error:
            raise DepsImageError(f"{error}; a prefix is a real directory, never a symlink") from None
        finally:
            os.close(parent)
        self._fds[prefix] = fd
        return fd

    def remove_prefix_root(self, prefix: str) -> None:
        """Delete a whole prefix (a whiteout of the prefix or its ancestor)."""
        parent = self._host_parent(prefix, False)
        self.forget(prefix)
        if parent is None:
            return
        try:
            _remove_at(parent, self.by_prefix[prefix][-1])
        finally:
            os.close(parent)

    def forget(self, prefix: str) -> None:
        fd = self._fds.pop(prefix, None)
        if fd is not None:
            os.close(fd)

    # -- entries below a prefix -------------------------------------------
    def parent_of(self, parts: tuple[str, ...], create: bool
                  ) -> Optional[tuple[int, str]]:
        """The (open parent directory descriptor, leaf name) of an entry
        strictly below a prefix, or `None` when a parent is absent and
        `create` is false. The caller closes the descriptor."""
        prefix = _prefix_of(parts, self.by_prefix)
        assert prefix is not None and len(parts) > len(self.by_prefix[prefix])
        root_fd = self.prefix_fd(prefix, create)
        if root_fd is None:
            return None
        below = parts[len(self.by_prefix[prefix]):]
        current = os.dup(root_fd)
        what = "layer path " + "/".join(parts)
        try:
            for component in below[:-1]:
                try:
                    following = _open_dir(current, component, what)
                except FileNotFoundError:
                    if not create:
                        os.close(current)
                        return None
                    os.mkdir(component, 0o777, dir_fd=current)
                    following = _open_dir(current, component, what)
                os.close(current)
                current = following
        except BaseException:
            os.close(current)
            raise
        return current, below[-1]

    def open_directory(self, parts: tuple[str, ...]) -> Optional[int]:
        """An open descriptor of the directory `parts` (a prefix root or
        below), or `None` when it is absent or a leaf symlink or file.
        Components above the leaf must be real directories."""
        prefix = _prefix_of(parts, self.by_prefix)
        assert prefix is not None
        if parts == self.by_prefix[prefix]:
            fd = self.prefix_fd(prefix, False)
            return None if fd is None else os.dup(fd)
        located = self.parent_of(parts, False)
        if located is None:
            return None
        parent, name = located
        try:
            return _open_dir(parent, name, "layer path " + "/".join(parts))
        except (FileNotFoundError, DepsImageError):
            return None                       # absent, or a leaf symlink or file
        finally:
            os.close(parent)

    def remove(self, parts: tuple[str, ...]) -> None:
        prefix = _prefix_of(parts, self.by_prefix)
        assert prefix is not None
        if parts == self.by_prefix[prefix]:
            self.remove_prefix_root(prefix)
            return
        located = self.parent_of(parts, False)
        if located is None:
            return
        parent, name = located
        try:
            _remove_at(parent, name)
        finally:
            os.close(parent)

    def clear(self, parts: tuple[str, ...]) -> None:
        """Empty the directory `parts` (an opaque whiteout). A leaf that is
        a symlink or file is left for this layer's own entry to replace;
        it is never followed to clear its target."""
        fd = self.open_directory(parts)
        if fd is None:
            return
        try:
            for child in os.listdir(fd):
                _remove_at(fd, child)
        finally:
            os.close(fd)


def apply_layer(layer: Path, root: Path,
                prefixes: tuple[str, ...] = CONSUMER_PREFIXES) -> int:
    """Apply one gzipped layer tar to `root`, limited to `prefixes`.

    Whiteouts first (they delete what LOWER layers wrote), then the
    entries; directory modes and times last, so writing into a directory
    does not disturb them. Device nodes and entries outside the prefixes
    are not written. Every name is decoded to plain components first
    (`_member_parts`, `_component`), and every operation goes through a
    `Jail` by descriptor. Returns the number of entries written."""
    try:
        return _apply_layer(layer, root, prefixes)
    except OSError as error:
        # an operating-system refusal (name too long, not empty, exists) on
        # a hostile layer is a refusal, not a crash
        raise DepsImageError(f"layer {layer.name}: {error}") from error


def _apply_layer(layer: Path, root: Path, prefixes: tuple[str, ...]) -> int:
    with Jail(root, prefixes) as jail, tarfile.open(layer, "r:*") as archive:
        by_prefix = jail.by_prefix
        members = [(member, _member_parts(member.name)) for member in archive.getmembers()]
        for member, parts in members:
            if not parts:
                continue
            base = parts[-1]
            directory = parts[:-1]
            if base == ".wh..wh..opq":
                if _prefix_of(directory, by_prefix) is not None:
                    jail.clear(directory)
                else:
                    for prefix in _ancestor_prefixes(directory, by_prefix):
                        jail.clear(by_prefix[prefix])
            elif base.startswith(".wh..wh."):
                continue                      # other archive metadata, never a victim
            elif base.startswith(".wh."):
                victim = directory + (_component(base[len(".wh."):],
                                                 f"whiteout {member.name!r}"),)
                if _prefix_of(victim, by_prefix) is not None:
                    jail.remove(victim)
                else:
                    for prefix in _ancestor_prefixes(victim, by_prefix):
                        jail.remove_prefix_root(prefix)
        written = 0
        directories: list[tuple[tuple[str, ...], tarfile.TarInfo]] = []
        for member, parts in members:
            if (not parts or parts[-1].startswith(".wh.")
                    or _prefix_of(parts, by_prefix) is None):
                continue
            prefix = _prefix_of(parts, by_prefix)
            if parts == by_prefix[prefix]:
                if not member.isdir():
                    raise DepsImageError(f"layer entry {member.name} is a prefix root but not "
                                         "a directory; a prefix is never a symlink or a file")
                jail.prefix_fd(prefix, True)
                directories.append((parts, member))
                written += 1
                continue
            located = jail.parent_of(parts, True)
            assert located is not None
            parent, name = located
            try:
                _write_entry(jail, archive, member, parts, parent, name)
            finally:
                os.close(parent)
            if member.isdir():
                directories.append((parts, member))
            written += 1
        for parts, member in reversed(directories):
            # Opened again by descriptor: a later entry may have replaced a
            # directory above this one with a symlink.
            if parts == by_prefix[_prefix_of(parts, by_prefix)]:
                fd = jail.prefix_fd(_prefix_of(parts, by_prefix), False)
                fd = None if fd is None else os.dup(fd)
            else:
                located = jail.parent_of(parts, False)
                fd = None
                if located is not None:
                    parent, name = located
                    try:
                        fd = _open_dir(parent, name, "layer path " + "/".join(parts))
                    finally:
                        os.close(parent)
            if fd is None:
                raise DepsImageError(f"directory {'/'.join(parts)} was replaced by a symlink")
            try:
                os.fchmod(fd, member.mode & 0o1777)
                os.utime(fd, (member.mtime, member.mtime))
            finally:
                os.close(fd)
    return written


def _write_entry(jail: Jail, archive: tarfile.TarFile, member: tarfile.TarInfo,
                 parts: tuple[str, ...], parent: int, name: str) -> None:
    """Write one non-prefix-root entry as the leaf `name` of `parent`."""
    if member.isdir():
        # A symlink or file in the way is replaced, never followed: the
        # mode set at the end must not reach outside the prefix.
        try:
            mode = os.stat(name, dir_fd=parent, follow_symlinks=False).st_mode
        except FileNotFoundError:
            mode = None
        if mode is not None and not stat.S_ISDIR(mode):
            _remove_at(parent, name)
        try:
            os.mkdir(name, 0o777, dir_fd=parent)
        except FileExistsError:
            pass
    elif member.issym():
        target = member.linkname
        if not target or "\0" in target:
            raise DepsImageError(f"symlink {member.name!r} has an empty or malformed target")
        # The target is stored verbatim and never followed: every later
        # operation resolves component by component with O_NOFOLLOW.
        _remove_at(parent, name)
        os.symlink(target, name, dir_fd=parent)
        os.utime(name, (member.mtime, member.mtime), dir_fd=parent, follow_symlinks=False)
    elif member.islnk():
        source = _member_parts(member.linkname, "hard link target")
        if not source or _prefix_of(source, jail.by_prefix) is None:
            raise DepsImageError(f"hard link {member.name} -> {member.linkname} "
                                 "leaves the prefixes")
        if source == jail.by_prefix[_prefix_of(source, jail.by_prefix)]:
            raise DepsImageError(f"hard link {member.name} names a prefix root")
        located = jail.parent_of(source, False)
        if located is None:
            raise DepsImageError(f"hard link {member.name}: source {member.linkname} is absent")
        source_parent, source_name = located
        try:
            _remove_at(parent, name)
            os.link(source_name, name, src_dir_fd=source_parent, dst_dir_fd=parent,
                    follow_symlinks=False)
        except OSError as error:
            raise DepsImageError(f"hard link {member.name}: {error}") from error
        finally:
            os.close(source_parent)
    elif member.isreg():
        extracted = archive.extractfile(member)
        assert extracted is not None
        _remove_at(parent, name)
        fd = os.open(name, _OPEN_NEW, 0o600, dir_fd=parent)
        with extracted, os.fdopen(fd, "wb") as out:
            shutil.copyfileobj(extracted, out, 1 << 20)
            out.flush()
            os.fchmod(out.fileno(), member.mode & 0o1777)
            os.utime(out.fileno(), (member.mtime, member.mtime))
    else:
        raise DepsImageError(f"layer entry {member.name} is a device or FIFO; refusing")


def parse_reference(reference: str) -> tuple[str, str, str]:
    """`host/repository:tag` (or `@sha256:...`) -> (host, repository, ref)."""
    host, _, rest = reference.partition("/")
    if "@" in rest:
        repository, _, ref = rest.partition("@")
    else:
        repository, _, ref = rest.rpartition(":")
    if not host or not repository or not ref:
        raise DepsImageError(f"cannot parse image reference {reference!r}")
    return host, repository, ref


def fetch(registry: Registry, reference: str, root: Path,
          prefixes: tuple[str, ...] = CONSUMER_PREFIXES,
          allow_existing: bool = False, log: Callable[[str], None] = print) -> dict:
    """Install the image's toolchain and store under `root` without Docker."""
    occupied = [p for p in prefixes
                if (root / p).exists() and any((root / p).iterdir())]
    if occupied and not allow_existing:
        raise DepsImageError(
            "refusing to unpack over existing " + ", ".join(
                str(root / p) for p in occupied)
            + "; start from empty prefixes (or pass --allow-existing)")
    resolved = resolve_image(registry, reference)
    state, document = resolved.image, resolved.document
    (config_digest, config_size), layer_descriptors = image_blobs(document, reference)
    try:
        config = json.loads(registry.blob_bytes(config_digest, config_size))
    except ValueError as error:
        raise RegistryError("the image config is not JSON") from error
    if (not isinstance(config, dict) or config.get("os") != "linux"
            or config.get("architecture") != "amd64"):
        raise DepsImageError("the image is not linux/amd64")
    # a tag is resolved exactly once, here: from now on every fetch is by the
    # digest it resolved to, and each is verified against that identity
    log(f"{reference} -> {resolved.top.digest}"
        + ("" if resolved.top.digest == state.digest else f" (linux/amd64 {state.digest})"))
    with tempfile.TemporaryDirectory(prefix="synarchy-deps-") as scratch:
        for index, (layer_digest, layer_size) in enumerate(layer_descriptors, 1):
            path = Path(scratch) / "layer"
            with open(path, "wb") as out:
                registry.download_blob(layer_digest, out.write, layer_size)
            count = apply_layer(path, root, prefixes)
            path.unlink()
            log(f"layer {index}/{len(layer_descriptors)} {layer_digest[:19]}: "
                f"{count} entries under {', '.join(prefixes)}")
    labels = (config.get("config") or {}).get("Labels") or {}
    return {"digest": state.digest, "labels": labels}


# ---------------------------------------------------------------------------
# Inside the image

def ghc_leftovers(ghc_dir: Path) -> list[str]:
    """Profiling libraries or bundled docs left in an installed GHC."""
    found = []
    for version_dir in sorted(ghc_dir.iterdir()) if ghc_dir.is_dir() else []:
        if (version_dir / "share" / "doc").exists():
            found.append(str(version_dir / "share" / "doc"))
        for dirpath, _dirs, files in os.walk(version_dir):
            found += [os.path.join(dirpath, f) for f in files
                      if f.endswith(PROFILING_SUFFIXES)]
    return found


def verify_image(checkout: Path, expect_tag: str, metadata_path: Path = Path(METADATA_PATH),
                 run: Callable[..., subprocess.CompletedProcess] = subprocess.run) -> list[str]:
    """Inside a candidate image: the toolchain, then a consumer's view.

    Copies the checkout to a scratch directory that is NOT the producer
    workspace, resolves there with the test suites enabled, and requires
    cabal to build nothing but the local packages and the compatibility
    check to pass."""
    metadata = json.loads(metadata_path.read_text())
    report = []
    if metadata.get("identity") != expect_tag:
        raise DepsImageError(f"image metadata names {metadata.get('identity')!r}, "
                             f"not {expect_tag!r}")
    leftovers = ghc_leftovers(Path(GHCUP_DIR) / "ghc")
    if leftovers:
        raise DepsImageError(f"GHC still carries profiling libraries or docs: {leftovers[0]}")
    report.append("GHC has no profiling libraries and no bundled docs")
    ghc = run(["ghc", "--numeric-version"], capture_output=True, text=True).stdout.strip()
    if f"ghc-{ghc}" != metadata["toolchain"]["compiler-id"]:
        raise DepsImageError(f"ghc reports {ghc}, metadata {metadata['toolchain']['compiler-id']}")
    with tempfile.TemporaryDirectory(prefix="consumer-") as scratch:
        work = Path(scratch) / "synarchy"
        shutil.copytree(checkout, work, symlinks=True,
                        ignore=shutil.ignore_patterns(".git", "dist-newstyle", "dist-*"))
        dry = run(["cabal", "build", "all", "--enable-tests", "--dry-run"], cwd=work,
                  capture_output=True, text=True)
        if dry.returncode != 0:
            raise DepsImageError(f"consumer dry-run failed: {dry.stderr.strip()[-400:]}")
        plan = json.loads((work / "dist-newstyle" / "cache" / "plan.json").read_text())
        leftover = dependency_builds(dry.stdout, plan)
        if leftover:
            raise DepsImageError("a fresh checkout would still build dependencies: "
                                 + ", ".join(leftover))
        problems = compatibility(plan, metadata)
        if problems:
            raise DepsImageError("compatibility check failed: " + "; ".join(problems[:5]))
        report.append(f"fresh checkout at {work}: dry-run builds only "
                      f"{', '.join(sorted(local_packages(plan)))}; "
                      f"{len(non_local_units(plan)['store'])} store units match")
    return report


def _write_github_output(path: Optional[str], values: dict[str, str]) -> None:
    if path:
        with open(path, "a", encoding="utf-8") as out:
            for key, value in values.items():
                out.write(f"{key}={value}\n")


# ---------------------------------------------------------------------------
# Command line

def _cmd_plan(args: argparse.Namespace) -> int:
    docker = Docker(args.context)
    base = docker.resolve_base(args.base_image, pull=not args.no_pull)
    with tempfile.TemporaryDirectory(prefix="synarchy-deps-plan-") as out:
        docker.build("plan-export", build_args(base), output=out)
        identity = plan_identity(Path(out, "plan.json").read_bytes(),
                                 Path(out, "os-release").read_text())
    require_publishing_platform(identity)
    print(f"identity: {identity.tag}")
    print(f"base image: {base}")
    _write_github_output(args.github_output, {
        "tag": identity.tag, "plan_sha256": identity.plan_sha256,
        "base_image": base})
    return 0


def _credentials() -> Optional[tuple[str, str]]:
    user, token = os.environ.get("GHCR_USERNAME", ""), os.environ.get("GHCR_TOKEN", "")
    return (user, token) if user and token else None


def _registry_clients(args: argparse.Namespace, what: str) -> tuple[Registry, Registry]:
    refusal = publication_refusal(args.event, args.ref, args.repository)
    if refusal:
        raise Refused(refusal)
    credentials = _credentials()
    if credentials is None:
        raise DepsImageError(f"GHCR_USERNAME and GHCR_TOKEN must be set to {what}")
    return (Registry(args.registry_url, credentials=credentials),
            Registry(args.registry_url))


def _cmd_publish(args: argparse.Namespace) -> int:
    registry, anonymous = _registry_clients(args, "publish")
    result = publish(
        registry=registry, anonymous=anonymous, docker=Docker(args.context),
        event=args.event, ref=args.ref,
        repository=args.repository, tag=args.tag, plan_sha256=args.plan_sha256,
        base_image=args.base_image, revision=args.sha, build_url=args.build_url,
        checkout=args.context)
    if args.github_summary:
        with open(args.github_summary, "a", encoding="utf-8") as out:
            out.write(f"### synarchy-deps\n\n- identity `{result.tag}` -> `{result.digest}`\n"
                      f"- {'built and pushed' if result.pushed else 'already published'}\n"
                      + "".join(f"- {line}\n" for line in result.anonymous + result.notes))
    return 0


def _cmd_reconcile_master(args: argparse.Namespace) -> int:
    registry, anonymous = _registry_clients(args, f"move {MOVING_TAG}")
    git = Git(args.context)
    result = reconcile_master(
        registry=registry, anonymous=anonymous, git=git,
        resolve_tip=lambda commit: resolve_tip_identity(
            git, Docker, args.base_image, commit),
        event=args.event, ref=args.ref, repository=args.repository,
        tag=args.tag, revision=args.sha)
    if args.github_summary:
        with open(args.github_summary, "a", encoding="utf-8") as out:
            out.write(f"### synarchy-deps `{MOVING_TAG}`\n\n"
                      f"- master's tip `{result.tip[:12]}` -> `{result.target}`\n"
                      f"- `{MOVING_TAG}`: {'moved' if result.moved else 'unchanged'} "
                      f"({result.reason})\n"
                      + "".join(f"- {line}\n" for line in result.anonymous + result.notes))
    return 0


def _cmd_store_metadata(args: argparse.Namespace) -> int:
    cabal = subprocess.run(["cabal", "--numeric-version"], capture_output=True,
                           text=True).stdout.strip()
    index = Path(CABAL_DIR, "index-state")
    metadata = store_metadata(
        Path(args.plan).read_bytes(), expect_plan_sha256=args.expect_plan_sha256,
        dry_run_text=Path(args.dry_run).read_text(),
        os_release_text=Path(args.os_release).read_text(),
        source_revision=args.source_revision, base_image=args.base_image,
        build_url=args.build_url, cabal_version=cabal,
        index_state=index.read_text().strip() if index.exists() else "")
    Path(args.output).write_text(json.dumps(metadata, indent=1, sort_keys=True) + "\n")
    print(f"store for {metadata['identity']}: {len(metadata['units']['store'])} units")
    return 0


def _cmd_verify_image(args: argparse.Namespace) -> int:
    for line in verify_image(Path(args.checkout), args.expect_tag):
        print(line)
    return 0


def _cmd_fetch(args: argparse.Namespace) -> int:
    host, repository, reference = parse_reference(args.ref)
    url = args.registry_url or f"https://{host}"
    result = fetch(Registry(url, repository), reference, Path(args.root),
                   allow_existing=args.allow_existing)
    metadata = Path(args.root, METADATA_PATH.lstrip("/"))
    if metadata.exists():
        data = json.loads(metadata.read_text())
        print(f"identity {data['identity']}, built from "
              f"{data['producer']['repository']}@{data['producer']['revision']}")
    print(f"installed {result['digest']}; now set:\n"
          f"  export PATH={GHCUP_DIR}/bin:$PATH CABAL_DIR={CABAL_DIR}")
    return 0


def _cmd_check(args: argparse.Namespace) -> int:
    plan = json.loads(Path(args.plan).read_text())
    metadata = json.loads(Path(args.metadata).read_text())
    problems = compatibility(plan, metadata)
    if problems:
        print(f"MISMATCH against {metadata.get('identity')}: cabal will build "
              f"{sum(p.startswith('not in the image') for p in problems)} unit(s) itself")
        for problem in problems:
            print(f"  {problem}")
        return 1
    units = non_local_units(plan)
    print(f"MATCH {metadata.get('identity')}: all {len(units['store'])} store units "
          f"and {len(units['compiler'])} compiler packages come from the image")
    return 0


def main(argv: Optional[list[str]] = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    sub = parser.add_subparsers(dest="command", required=True)

    plan = sub.add_parser("plan", help="resolve the 24.04 plan; print its identity tag")
    plan.add_argument("--base-image", default="ubuntu:24.04")
    plan.add_argument("--no-pull", action="store_true",
                      help="use a local base image as-is (local testing)")
    plan.add_argument("--context", default=".")
    plan.add_argument("--github-output")
    plan.set_defaults(func=_cmd_plan)

    pub = sub.add_parser("publish", help="publish this run's immutable tag (once)")
    for name in ("tag", "plan-sha256", "base-image", "event", "ref", "repository", "sha"):
        pub.add_argument(f"--{name}", required=True)
    pub.add_argument("--build-url", default="")
    pub.add_argument("--context", default=".")
    pub.add_argument("--registry-url", default=f"https://{REGISTRY}")
    pub.add_argument("--github-summary")
    pub.set_defaults(func=_cmd_publish)

    rec = sub.add_parser("reconcile-master",
                         help=f"point {MOVING_TAG} at master's tip's identity "
                              f"(under the {MASTER_LOCK_GROUP} lock)")
    for name in ("tag", "base-image", "event", "ref", "repository", "sha"):
        rec.add_argument(f"--{name}", required=True)
    rec.add_argument("--context", default=".")
    rec.add_argument("--registry-url", default=f"https://{REGISTRY}")
    rec.add_argument("--github-summary")
    rec.set_defaults(func=_cmd_reconcile_master)

    meta = sub.add_parser("store-metadata", help="(image build) check the store, write metadata")
    for name in ("plan", "expect-plan-sha256", "dry-run", "os-release", "output"):
        meta.add_argument(f"--{name}", required=True)
    for name in ("source-revision", "base-image", "build-url"):
        meta.add_argument(f"--{name}", default="")
    meta.set_defaults(func=_cmd_store_metadata)

    ver = sub.add_parser("verify-image", help="(inside a candidate) consumer-side checks")
    ver.add_argument("--checkout", required=True)
    ver.add_argument("--expect-tag", required=True)
    ver.set_defaults(func=_cmd_verify_image)

    fet = sub.add_parser("fetch", help="install toolchain + store without Docker")
    fet.add_argument("--ref", default=DEFAULT_REF)
    fet.add_argument("--root", default="/")
    fet.add_argument("--registry-url", help="override https://<host> (testing)")
    fet.add_argument("--allow-existing", action="store_true")
    fet.set_defaults(func=_cmd_fetch)

    chk = sub.add_parser("check", help="compare a checkout's plan with the image")
    chk.add_argument("--plan", default="dist-newstyle/cache/plan.json")
    chk.add_argument("--metadata", default=METADATA_PATH)
    chk.set_defaults(func=_cmd_check)

    args = parser.parse_args(argv)
    try:
        return args.func(args)
    except Refused as error:
        print(f"ci_deps_image: refused: {error}", file=sys.stderr)
        return 3
    except DepsImageError as error:
        print(f"ci_deps_image: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
