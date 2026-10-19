#!/usr/bin/env python3
"""Tests for tools/ci_deps_image.py (#2864) and its workflow wiring.

Engine-free, network-free (one in-process fake registry on 127.0.0.1),
a few seconds. Run by static-audits and tools/ci-local.sh.

What is pinned
--------------
* Identity: the plan hash with its platform marker; identical plans give
  identical tags and an A -> B -> A rollback gives A's tag again; a plan
  resolved anywhere but the fixed workspace is refused.
* Publication decisions over a fake registry, docker and git: an unseen
  identity is built and pushed once, a seen one never again; A -> B -> A
  reuses A and moves `master` back; a run of the same identity that
  published during this run's build is kept, not overwritten; a retry
  after a failed moving-tag update recovers it; registry errors are not
  absence; an older run never moves `master` back past a newer plan, while
  code- and docs-only commits after it use its identity without resolving
  the tip; pull requests (fork and same-repository), other refs and other
  repositories write nothing.
  `master` names the identity of master's tip: the review's interleaving
  (A checks, B publishes and moves, A writes) and its mirror end on B, a
  push during a decision is repaired by the re-check, and a reconcile that
  replaced a cancelled newer one still reaches the newer plan.
* Anonymous access: both tags' manifests and blobs, and the private case.
* Docker-free consumption: layer order, whiteouts and opaque directories,
  links, prefix confinement, path and symlink escapes, digest checks, the
  CDN redirect without credentials, and refusing non-empty prefixes.
* Consumer compatibility: matching plans from another directory, and
  mismatching ones (a changed unit, another compiler).
* Wiring: deps-image.yml runs only for master pushes of this repository
  (checked with the parity audit's own scheduling model), keys the publish
  lock on the identity and the `master` lock on nothing (one fixed group,
  held by the only job that writes `master`), grants packages: write only
  to those two jobs; the
  Dockerfile's apt list, toolchain versions and plan inputs match the CI
  image and the repository.
"""
from __future__ import annotations

import hashlib
import http.server
import io
import json
import os
import random
import re
import shutil
import stat
import subprocess
import sys
import tarfile
import tempfile
import threading
import unittest
import urllib.parse
import uuid
from pathlib import Path, PurePosixPath

TOOLS = Path(__file__).resolve().parent
REPO = TOOLS.parent
sys.path.insert(0, str(TOOLS))

import ci_deps_image as deps  # noqa: E402

MEDIA_MANIFEST = "application/vnd.docker.distribution.manifest.v2+json"
MEDIA_LAYER = "application/vnd.docker.image.rootfs.diff.tar.gzip"
MEDIA_CONFIG = "application/vnd.docker.container.image.v1+json"


def sha(data: bytes) -> str:
    return "sha256:" + hashlib.sha256(data).hexdigest()


# ---------------------------------------------------------------------------
# Fake registry: the ghcr.io behaviors the tool depends on.

class FakeRegistry:
    """Token challenge, 404 MANIFEST_UNKNOWN/NAME_UNKNOWN, ghcr's 403 DENIED
    for anonymous tokens on private or missing packages, blob redirects to a
    CDN that rejects an Authorization header, manifest PUT and the upload
    API `docker push` uses. Faults are injected per request."""

    def __init__(self, credentials=("bot", "s3cret"), redirect_blobs=True,
                 allow_anonymous_push=False):
        self.credentials = credentials
        self.redirect_blobs = redirect_blobs
        self.allow_anonymous_push = allow_anonymous_push
        self.repos: dict[str, dict] = {}
        self.tokens: dict[str, tuple[str, set[str]]] = {}
        self.faults: list[tuple[str, str, int, bytes]] = []
        self.requests: list[tuple[str, str]] = []
        self.uploads: dict[str, bytearray] = {}
        self.lock = threading.Lock()
        self.scheme = "http"
        self.refresh_tokens: set[str] = set()
        self.manifest_puts: list[tuple[str, str]] = []
        handler = self._handler()
        self.server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), handler)
        self.url = f"http://127.0.0.1:{self.server.server_address[1]}"
        self.thread = threading.Thread(target=self.server.serve_forever,
                                       kwargs={"poll_interval": 0.02}, daemon=True)
        self.thread.start()

    def close(self):
        self.server.shutdown()
        self.server.server_close()

    # -- state helpers ----------------------------------------------------
    def repo(self, name, create=True):
        if name not in self.repos and create:
            self.repos[name] = {"manifests": {}, "blobs": {}, "public": False}
        return self.repos.get(name)

    def put_image(self, name, tag, layers, labels=None, public=None):
        repo = self.repo(name)
        if public is not None:
            repo["public"] = public
        layer_entries, diff_ids = [], []
        for blob in layers:
            repo["blobs"][sha(blob)] = blob
            layer_entries.append({"mediaType": MEDIA_LAYER, "size": len(blob),
                                  "digest": sha(blob)})
            diff_ids.append(sha(tarfile_bytes_uncompressed(blob)))
        config = json.dumps({"architecture": "amd64", "os": "linux",
                             "config": {"Labels": labels or {}},
                             "rootfs": {"type": "layers", "diff_ids": diff_ids}}).encode()
        repo["blobs"][sha(config)] = config
        manifest = json.dumps({"schemaVersion": 2, "mediaType": MEDIA_MANIFEST,
                               "config": {"mediaType": MEDIA_CONFIG, "size": len(config),
                                          "digest": sha(config)},
                               "layers": layer_entries}).encode()
        repo["manifests"][tag] = (manifest, MEDIA_MANIFEST)
        repo["manifests"][sha(manifest)] = (manifest, MEDIA_MANIFEST)
        return sha(manifest)

    def digest_of(self, name, tag):
        entry = (self.repos.get(name) or {}).get("manifests", {}).get(tag)
        return sha(entry[0]) if entry else None

    def fault(self, method, pattern, status, codes=()):
        body = json.dumps({"errors": [{"code": c, "message": c} for c in codes]}).encode() \
            if codes else b""
        self.faults.append((method, pattern, status, body))

    def writes(self):
        return [r for r in self.requests if r[0] in ("PUT", "POST", "PATCH", "DELETE")]

    # -- HTTP -------------------------------------------------------------
    def _handler(self):
        registry = self

        class Handler(http.server.BaseHTTPRequestHandler):
            protocol_version = "HTTP/1.1"

            def log_message(self, *args):
                pass

            def _send(self, status, body=b"", headers=None, codes=()):
                if codes and not body:
                    body = json.dumps({"errors": [{"code": c, "message": c.lower()}
                                                  for c in codes]}).encode()
                self.send_response(status)
                for key, value in (headers or {}).items():
                    self.send_header(key, value)
                self.send_header("Content-Length", str(len(body)))
                if body and "Content-Type" not in (headers or {}):
                    self.send_header("Content-Type", "application/json")
                self.end_headers()
                if self.command != "HEAD":
                    self.wfile.write(body)

            def _body(self):
                if self.headers.get("Transfer-Encoding", "").lower() == "chunked":
                    data = bytearray()
                    while True:
                        size = int(self.rfile.readline().split(b";")[0].strip() or b"0", 16)
                        if size == 0:
                            self.rfile.readline()
                            return bytes(data)
                        data += self.rfile.read(size)
                        self.rfile.readline()
                length = int(self.headers.get("Content-Length") or 0)
                return self.rfile.read(length) if length else b""

            def _dispatch(self):
                parsed = urllib.parse.urlparse(self.path)
                with registry.lock:
                    registry.requests.append((self.command, parsed.path))
                    for index, (method, pattern, status, body) in enumerate(registry.faults):
                        if method == self.command and re.search(pattern, parsed.path):
                            del registry.faults[index]
                            self._body()
                            return self._send(status, body)
                if parsed.path == "/token":
                    return self._token(parsed)
                if parsed.path.startswith("/cdn/"):
                    if self.headers.get("Authorization"):
                        return self._send(400, b"CDN URLs are pre-signed; no Authorization")
                    digest = parsed.path[len("/cdn/"):]
                    for repo in registry.repos.values():
                        if digest in repo["blobs"]:
                            return self._send(200, repo["blobs"][digest],
                                              {"Content-Type": "application/octet-stream"})
                    return self._send(404, codes=("BLOB_UNKNOWN",))
                if parsed.path in ("/v2", "/v2/"):
                    # `docker login`'s probe: challenge, then accept any token.
                    auth = self.headers.get("Authorization", "")
                    if auth.startswith("Bearer ") and auth[7:] in registry.tokens:
                        return self._send(200, b"{}")
                    host = self.headers.get("Host")
                    return self._send(401, headers={"WWW-Authenticate":
                                      f'Bearer realm="{registry.scheme}://{host}/token",service="fake"'},
                                      codes=("UNAUTHORIZED",))
                match = re.fullmatch(r"/v2/(.+?)/(manifests|blobs)/(.+)", parsed.path)
                if not match:
                    return self._send(404, codes=("NOT_FOUND",))
                name, kind, rest = match.groups()
                actions = self._authorize(name)
                if actions is None:
                    return
                if kind == "manifests":
                    return self._manifests(name, rest, actions)
                return self._blobs(name, rest, actions, parsed)

            def _token(self, parsed):
                query = urllib.parse.parse_qs(parsed.query)
                form = (urllib.parse.parse_qs(self._body().decode())
                        if self.command == "POST" else {})
                scopes = query.get("scope") or form.get("scope") or [""]
                scopes = [s for scope in scopes for s in scope.split(" ")]
                _, name, _ = (scopes[0].split(":") + ["", "", ""])[:3]
                wanted = ",".join(part.split(":")[2] for part in scopes
                                  if part.count(":") == 2 and part.split(":")[1] == name)
                auth = self.headers.get("Authorization", "")
                user, secret = registry.credentials
                import base64
                basic = "Basic " + base64.b64encode(f"{user}:{secret}".encode()).decode()
                password_grant = (form.get("username") == [user]
                                  and form.get("password") == [secret])
                refresh_grant = (form.get("grant_type") == ["refresh_token"]
                                 and form.get("refresh_token", [""])[0] in registry.refresh_tokens)
                authenticated = auth == basic or password_grant or refresh_grant
                if authenticated:
                    granted = set(wanted.split(",")) if wanted else {"pull", "push"}
                else:
                    repo = registry.repos.get(name)
                    if repo is None or not repo["public"]:
                        return self._send(403, codes=("DENIED",))
                    granted = {"pull"}
                token = uuid.uuid4().hex
                registry.tokens[token] = (name, granted)
                reply = {"token": token, "access_token": token}
                if authenticated:
                    reply["refresh_token"] = refresh = uuid.uuid4().hex
                    registry.refresh_tokens.add(refresh)
                self._send(200, json.dumps(reply).encode())

            def _authorize(self, name):
                auth = self.headers.get("Authorization", "")
                token = auth[len("Bearer "):] if auth.startswith("Bearer ") else ""
                grant = registry.tokens.get(token)
                if grant is None or (grant[0] and grant[0] != name):
                    if registry.allow_anonymous_push:
                        return {"pull", "push"}
                    host = self.headers.get("Host")
                    self._send(401, headers={"WWW-Authenticate":
                               f'Bearer realm="{registry.scheme}://{host}/token",service="fake",'
                               f'scope="repository:{name}:pull"'}, codes=("UNAUTHORIZED",))
                    return None
                return grant[1]

            def _manifests(self, name, ref, actions):
                repo = registry.repo(name, create=self.command == "PUT")
                if self.command in ("GET", "HEAD"):
                    if repo is None:
                        return self._send(404, codes=("NAME_UNKNOWN",))
                    entry = repo["manifests"].get(ref)
                    if entry is None:
                        return self._send(404, codes=("MANIFEST_UNKNOWN",))
                    return self._send(200, entry[0], {"Content-Type": entry[1],
                                                       "Docker-Content-Digest": sha(entry[0])})
                if self.command == "PUT":
                    body = self._body()
                    if "push" not in actions:
                        return self._send(403, codes=("DENIED",))
                    media = self.headers.get("Content-Type", MEDIA_MANIFEST)
                    with registry.lock:
                        repo["manifests"][ref] = (body, media)
                        repo["manifests"][sha(body)] = (body, media)
                        registry.manifest_puts.append((ref, sha(body)))
                    return self._send(201, headers={"Docker-Content-Digest": sha(body),
                                                    "Location": f"/v2/{name}/manifests/{sha(body)}"})
                return self._send(405)

            def _blobs(self, name, rest, actions, parsed):
                repo = registry.repo(name, create=self.command != "GET" and self.command != "HEAD")
                if rest.startswith("uploads"):
                    if "push" not in actions:
                        self._body()
                        return self._send(403, codes=("DENIED",))
                    upload = rest[len("uploads/"):].strip("/")
                    if self.command == "POST":
                        self._body()
                        new = uuid.uuid4().hex
                        registry.uploads[new] = bytearray()
                        return self._send(202, headers={
                            "Location": f"/v2/{name}/blobs/uploads/{new}",
                            "Docker-Upload-UUID": new, "Range": "0-0"})
                    if upload not in registry.uploads:
                        self._body()
                        return self._send(404, codes=("BLOB_UPLOAD_UNKNOWN",))
                    data = registry.uploads[upload]
                    data += self._body()
                    if self.command == "PATCH":
                        return self._send(202, headers={
                            "Location": f"/v2/{name}/blobs/uploads/{upload}",
                            "Docker-Upload-UUID": upload, "Range": f"0-{max(len(data) - 1, 0)}"})
                    if self.command == "PUT":
                        digest = urllib.parse.parse_qs(parsed.query).get("digest", [""])[0]
                        if sha(bytes(data)) != digest:
                            return self._send(400, codes=("DIGEST_INVALID",))
                        repo["blobs"][digest] = bytes(data)
                        del registry.uploads[upload]
                        return self._send(201, headers={"Location": f"/v2/{name}/blobs/{digest}",
                                                        "Docker-Content-Digest": digest})
                    return self._send(405)
                if repo is None or rest not in repo["blobs"]:
                    return self._send(404, codes=("BLOB_UNKNOWN",))
                if registry.redirect_blobs:
                    return self._send(307, headers={"Location": f"/cdn/{rest}?sig=abc"})
                blob = repo["blobs"][rest]
                return self._send(200, blob, {"Content-Type": "application/octet-stream",
                                              "Docker-Content-Digest": rest})

            do_GET = do_HEAD = do_PUT = do_POST = do_PATCH = _dispatch

        return Handler


def tarfile_bytes_uncompressed(blob: bytes) -> bytes:
    import gzip
    try:
        return gzip.decompress(blob)
    except OSError:
        return blob


def make_layer(entries) -> bytes:
    """entries: (name, kind, payload, mode) with kind file|dir|symlink|hardlink|fifo."""
    raw = io.BytesIO()
    with tarfile.open(fileobj=raw, mode="w:gz") as archive:
        for name, kind, payload, mode in entries:
            info = tarfile.TarInfo(name)
            info.mode = mode
            info.mtime = 1_700_000_000
            data = None
            if kind == "file":
                data = payload.encode() if isinstance(payload, str) else payload
                info.size = len(data)
            elif kind == "dir":
                info.type = tarfile.DIRTYPE
            elif kind == "symlink":
                info.type, info.linkname = tarfile.SYMTYPE, payload
            elif kind == "hardlink":
                info.type, info.linkname = tarfile.LNKTYPE, payload
            elif kind == "fifo":
                info.type = tarfile.FIFOTYPE
            archive.addfile(info, io.BytesIO(data) if data is not None else None)
    return raw.getvalue()


# ---------------------------------------------------------------------------
# Plans

def unit(name, version, digest, style="global", deps_=()):
    return {"type": "configured", "id": f"{name}-{version}-{digest}", "pkg-name": name,
            "pkg-version": version, "style": style,
            "pkg-src": {"type": "repo-tar", "repo": {"type": "secure-repo",
                                                     "uri": "http://hackage.haskell.org/"}},
            "depends": list(deps_)}


def make_plan(workspace=deps.WORKSPACE, units=None, compiler="ghc-9.12.2", abi="9b33"):
    units = units if units is not None else [
        unit("zlib", "0.7.1.1", "aaa"), unit("vulkan", "3.26.6", "bbb"),
        unit("alex", "3.5.4.2", "e-alex-ccc")]
    # Listed by plan.json, needed by no local component: never in the store.
    unneeded = unit("constraints-extras", "0.4.0.2", "e-readme-fff")
    local = {"type": "configured", "id": "synarchy-0.1.0.0-inplace", "pkg-name": "synarchy",
             "pkg-version": "0.1.0.0", "style": "local",
             "pkg-src": {"type": "local", "path": f"{workspace}/."},
             "dist-dir": f"{workspace}/dist-newstyle/build/x86_64-linux/{compiler}/synarchy-0.1.0.0",
             "components": {"lib": {"depends": [u["id"] for u in units[:1]]
                                    + ["base-4.21.0.0-ae91"],
                                    "exe-depends": []},
                            "test:synarchy-test-headless": {
                                    "depends": [u["id"] for u in units[1:2]],
                                    "exe-depends": [u["id"] for u in units[2:]], "bin-file": None},
                            "exe:synarchy": {"depends": [u["id"] for u in units[1:2]],
                                    "exe-depends": [], "bin-file": None}}}
    exe = units[-1]
    exe["bin-file"] = f"{deps.STORE_DIR}/{compiler}-{abi}/{exe['id']}/bin/alex"
    plan = {"cabal-version": "3.16.1.0", "cabal-lib-version": "3.16.1.0",
            "compiler-id": compiler, "compiler-abi": abi, "os": "linux", "arch": "x86_64",
            "install-plan": [{"type": "pre-existing", "id": "base-4.21.0.0-ae91",
                              "pkg-name": "base", "pkg-version": "4.21.0.0", "depends": []},
                             {"type": "pre-existing", "id": "unused-1.0-0000",
                              "pkg-name": "unused", "pkg-version": "1.0", "depends": []}]
            + units + [unneeded, local]}
    return json.dumps(plan, indent=1).encode()


OS_2404 = 'PRETTY_NAME="Ubuntu 24.04.5 LTS"\nID=ubuntu\nVERSION_ID="24.04"\n'
OS_2204 = 'PRETTY_NAME="Ubuntu 22.04.5 LTS"\nID=ubuntu\nVERSION_ID="22.04"\n'


class IdentityTests(unittest.TestCase):
    def test_tag_is_platform_marked_plan_hash(self):
        plan = make_plan()
        identity = deps.plan_identity(plan, OS_2404)
        self.assertEqual(identity.tag, "ubuntu24.04-x86_64-ghc9.12.2-"
                         + hashlib.sha256(plan).hexdigest())
        self.assertFalse(identity.tag.startswith("deps-v2-"))

    def test_identical_plans_and_rollback_reproduce_the_tag(self):
        a, b = make_plan(), make_plan(units=[unit("zlib", "0.7.1.2", "ddd")])
        tags = [deps.plan_identity(p, OS_2404).tag for p in (a, b, a)]
        self.assertEqual(tags[0], tags[2])
        self.assertNotEqual(tags[0], tags[1])

    def test_plan_resolved_elsewhere_is_refused(self):
        for workspace in ("/home/runner/work/synarchy/synarchy", "/tmp/tmp.x1/synarchy"):
            with self.assertRaisesRegex(deps.DepsImageError, "fixed workspace"):
                deps.plan_identity(make_plan(workspace=workspace), OS_2404)

    def test_only_ubuntu_2404_publishes(self):
        deps.require_publishing_platform(deps.plan_identity(make_plan(), OS_2404))
        with self.assertRaisesRegex(deps.DepsImageError, "24.04"):
            deps.require_publishing_platform(deps.plan_identity(make_plan(), OS_2204))
        self.assertNotEqual(deps.plan_identity(make_plan(), OS_2204).tag,
                            deps.plan_identity(make_plan(), OS_2404).tag)


DRY_UP_TO_DATE = "Resolving dependencies...\nUp to date\n"
DRY_LOCAL_ONLY = ("Resolving dependencies...\nBuild profile: -w ghc-9.12.2 -O1\n"
                  "In order, the following would be built (use -v for more details):\n"
                  " - synarchy-0.1.0.0 (lib) (first run)\n"
                  " - synarchy-0.1.0.0 (exe:synarchy) (first run)\n")
DRY_WITH_DEPS = ("In order, the following would be built (use -v for more details):\n"
                 " - zlib-0.7.1.1 (lib) (requires build)\n"
                 " - JuicyPixels-3.3.9 (lib) (requires build)\n"
                 " - synarchy-0.1.0.0 (lib) (first run)\n")


class StoreTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.store = Path(self.tmp.name) / "store"
        self.plan = make_plan()
        for unit_id in deps.non_local_units(json.loads(self.plan))["store"]:
            (self.store / "ghc-9.12.2-9b33" / unit_id).mkdir(parents=True)

    def tearDown(self):
        self.tmp.cleanup()

    def metadata(self, **overrides):
        args = dict(expect_plan_sha256=hashlib.sha256(self.plan).hexdigest(),
                    dry_run_text=DRY_LOCAL_ONLY, os_release_text=OS_2404,
                    source_revision="abc123", base_image="ubuntu@sha256:00",
                    build_url="https://example/run/1", cabal_version="3.16.1.0",
                    store_dir=str(self.store))
        args.update(overrides)
        return deps.store_metadata(self.plan, **args)

    def test_dry_run_parsing(self):
        plan = json.loads(self.plan)
        self.assertEqual(deps.dependency_builds(DRY_UP_TO_DATE, plan), [])
        self.assertEqual(deps.dependency_builds(DRY_LOCAL_ONLY, plan), [])
        self.assertEqual(deps.dependency_builds(DRY_WITH_DEPS, plan),
                         ["zlib-0.7.1.1", "JuicyPixels-3.3.9"])

    def test_metadata_records_provenance_and_units(self):
        data = self.metadata()
        self.assertEqual(data["identity"], deps.plan_identity(self.plan, OS_2404).tag)
        self.assertEqual(data["producer"]["revision"], "abc123")
        self.assertEqual(data["producer"]["base_image"], "ubuntu@sha256:00")
        self.assertEqual(data["toolchain"]["compiler-id"], "ghc-9.12.2")
        self.assertEqual(data["platform"]["distro"], "ubuntu24.04")
        self.assertEqual(len(data["units"]["store"]), 3)
        self.assertEqual(data["units"]["compiler"], ["base-4.21.0.0-ae91"])
        self.assertNotIn("constraints-extras-0.4.0.2-e-readme-fff", data["units"]["store"])

    def test_store_is_bound_to_its_plan(self):
        with self.assertRaisesRegex(deps.DepsImageError, "not the"):
            self.metadata(expect_plan_sha256="0" * 64)
        with self.assertRaisesRegex(deps.DepsImageError, "still build"):
            self.metadata(dry_run_text=DRY_WITH_DEPS)
        (self.store / "ghc-9.12.2-9b33" / "zlib-0.7.1.1-aaa").rmdir()
        with self.assertRaisesRegex(deps.DepsImageError, "missing"):
            self.metadata()

    def test_compatibility_match_and_mismatch(self):
        data = self.metadata()
        elsewhere = json.loads(self.plan.replace(deps.WORKSPACE.encode(), b"/home/me/synarchy"))
        self.assertEqual(deps.compatibility(elsewhere, data), [])
        changed = json.loads(make_plan(units=[unit("zlib", "0.7.1.1", "zzz"),
                                              unit("vulkan", "3.26.6", "bbb"),
                                              unit("alex", "3.5.4.2", "e-alex-ccc")]))
        problems = deps.compatibility(changed, data)
        self.assertEqual(problems, ["not in the image (store): zlib-0.7.1.1-zzz"])
        other = json.loads(make_plan(compiler="ghc-9.14.1", abi="1234"))
        self.assertTrue(any(p.startswith("compiler:") for p in deps.compatibility(other, data)))


# ---------------------------------------------------------------------------
# Publication over fakes

class FakeDocker:
    def __init__(self, registry: FakeRegistry, on_build=None):
        self.registry = registry
        self.on_build = on_build
        self.builds, self.verified, self.pushed = [], [], []
        self.counter = 0

    def build(self, target, build_args, labels=None, tag=None, output=None):
        self.counter += 1
        self.builds.append((target, dict(build_args), dict(labels or {}), tag))
        if self.on_build:
            self.on_build()

    def verify(self, image, tag, checkout):
        self.verified.append((image, tag))

    def push(self, local, remote):
        _, _, tag = remote.rpartition(":")
        labels = self.builds[-1][2]
        layer = make_layer([("usr/local/cabal/store/marker", "file",
                             f"{tag} build {self.counter} {uuid.uuid4().hex}", 0o644)])
        self.registry.put_image(deps.REPOSITORY, tag, [layer], labels)
        self.pushed.append(remote)


class FakeGit:
    """A linear master history. `plans` maps the commits that set a plan
    (and so change the plan inputs) to its plan.json bytes; every other
    commit inherits its parent's plan. `edits` are commits that change a
    plan input without changing the resolved plan. `on_tip[n]` runs just
    before the n-th `master_tip()` call returns (1-based): a push landing
    at exactly that moment."""

    def __init__(self, history, plans=None, edits=()):
        self.history = list(history)
        self.plans = dict(plans or {})
        self.edits = set(edits)
        self.on_tip: dict[int, object] = {}
        self.tip_calls = 0

    def push(self, commit, plan=None, edit=False):
        self.history.append(commit)
        if plan is not None:
            self.plans[commit] = plan
        if edit:
            self.edits.add(commit)

    def plan_of(self, commit):
        for candidate in reversed(self.history[:self.history.index(commit) + 1]):
            if candidate in self.plans:
                return self.plans[candidate]
        raise AssertionError(f"no plan for {commit}")

    def master_tip(self):
        self.tip_calls += 1
        hook = self.on_tip.pop(self.tip_calls, None)
        if hook:
            hook()
        return self.history[-1]

    def is_ancestor(self, older, newer):
        return older in self.history and newer in self.history and \
            self.history.index(older) <= self.history.index(newer)

    def plan_inputs_changed(self, older, newer):
        span = self.history[self.history.index(older) + 1:self.history.index(newer) + 1]
        return any(commit in self.plans or commit in self.edits for commit in span)


class HookedRegistry(deps.Registry):
    """A credentialed client that runs `before_put` (once) just before its
    next manifest PUT: the moment between a reconcile's decision and its
    write."""

    before_put = None

    def put_manifest(self, reference, body, media_type):
        hook, self.before_put = self.before_put, None
        if hook:
            hook()
        return super().put_manifest(reference, body, media_type)


PLAN_A = make_plan()
PLAN_B = make_plan(units=[unit("zlib", "0.7.1.2", "ddd")])
PLAN_C = make_plan(units=[unit("vulkan", "3.26.7", "eee")])


def tag_of(plan):
    return deps.plan_identity(plan, OS_2404).tag


class PublishTests(unittest.TestCase):
    """The per-identity job: the immutable tag, written at most once."""

    def setUp(self):
        self.fake = FakeRegistry()
        self.fake.repo(deps.REPOSITORY)["public"] = True
        self.docker = FakeDocker(self.fake)
        self.logs: list[str] = []

    def tearDown(self):
        self.fake.close()

    def run_publish(self, commit, plan=None, event="push", ref="refs/heads/master",
                    repository="coghex/synarchy"):
        identity = deps.plan_identity(plan or PLAN_A, OS_2404)
        return deps.publish(registry=deps.Registry(self.fake.url,
                                                   credentials=self.fake.credentials),
                            anonymous=deps.Registry(self.fake.url), docker=self.docker,
                            event=event, ref=ref, repository=repository,
                            tag=identity.tag, plan_sha256=identity.plan_sha256,
                            base_image="ubuntu@sha256:" + "1" * 64, revision=commit,
                            build_url="https://example/run", log=self.logs.append)

    def test_new_identity_publishes_once_and_leaves_master_alone(self):
        result = self.run_publish("c1")
        self.assertTrue(result.built and result.pushed)
        self.assertEqual(self.fake.digest_of(deps.REPOSITORY, result.tag), result.digest)
        self.assertIsNone(self.fake.digest_of(deps.REPOSITORY, deps.MOVING_TAG))
        self.assertNotIn(deps.MOVING_TAG, [tag for tag, _ in self.fake.manifest_puts])
        self.assertEqual(self.docker.builds[0][1]["EXPECTED_PLAN_SHA256"],
                         result.tag.rsplit("-", 1)[1])
        labels = self.docker.builds[0][2]
        self.assertEqual(labels["org.opencontainers.image.revision"], "c1")
        self.assertEqual(labels["org.opencontainers.image.base.digest"], "sha256:" + "1" * 64)
        self.assertEqual(len(result.anonymous), 1)

    def test_unchanged_identity_publishes_nothing(self):
        first = self.run_publish("c1")
        before = len(self.fake.writes())
        second = self.run_publish("c2")
        self.assertFalse(second.built or second.pushed)
        self.assertEqual(second.digest, first.digest)
        self.assertEqual(len(self.docker.builds), 1)
        self.assertEqual(len(self.fake.writes()), before)

    def test_concurrent_run_publishing_first_is_kept(self):
        tag = tag_of(PLAN_A)
        layer = make_layer([("usr/local/cabal/store/other-run", "file", "x", 0o644)])
        winner = []
        self.docker.on_build = lambda: winner.append(
            self.fake.put_image(deps.REPOSITORY, tag, [layer]))
        result = self.run_publish("c1")
        self.assertTrue(result.built)
        self.assertFalse(result.pushed)
        self.assertEqual(result.digest, winner[0])
        self.assertEqual(self.fake.digest_of(deps.REPOSITORY, tag), winner[0])
        self.assertIn("kept its image", " ".join(result.notes))

    def test_registry_errors_are_not_absence(self):
        for status, codes in ((500, ()), (429, ("TOOMANYREQUESTS",)), (404, ()),
                              (403, ("DENIED",)), (502, ())):
            self.fake.fault("GET", r"/manifests/ubuntu24\.04", status, codes)
            with self.assertRaises(deps.DepsImageError, msg=f"{status} {codes}"):
                self.run_publish("c1")
        self.assertEqual(self.docker.builds, [])
        self.assertEqual(self.fake.writes(), [])

    def test_name_unknown_is_absence(self):
        self.fake.repos.clear()
        registry = deps.Registry(self.fake.url, credentials=self.fake.credentials)
        self.assertFalse(registry.manifest("anything").present)

    def test_pull_requests_and_other_refs_write_nothing(self):
        for event, ref, repository in REFUSED_EVENTS:
            with self.assertRaises(deps.Refused, msg=event):
                self.run_publish("c1", event=event, ref=ref, repository=repository)
        self.assertEqual(self.fake.requests, [])
        self.assertEqual(self.docker.builds, [])

    def test_private_package_fails_the_anonymous_check(self):
        self.fake.repo(deps.REPOSITORY)["public"] = False
        with self.assertRaisesRegex(deps.NotAnonymous, "Change visibility"):
            self.run_publish("c1")

    def test_cli_refuses_a_pull_request_before_any_request(self):
        import contextlib
        for command in (["publish", "--plan-sha256", "p"], ["reconcile-master"]):
            self.fake.requests.clear()
            with contextlib.redirect_stderr(io.StringIO()) as err:
                code = deps.main(command + [
                    "--tag", "t", "--base-image", "b", "--event", "pull_request",
                    "--ref", "refs/pull/1/merge", "--repository", "coghex/synarchy",
                    "--sha", "c1", "--registry-url", self.fake.url])
            self.assertIn("pull requests never do", err.getvalue(), command[0])
            self.assertEqual(code, 3, command[0])
            self.assertEqual(self.fake.requests, [], command[0])


REFUSED_EVENTS = (("pull_request", "refs/pull/7/merge", "coghex/synarchy"),
                  ("pull_request_target", "refs/heads/master", "coghex/synarchy"),
                  ("push", "refs/heads/feature", "coghex/synarchy"),
                  ("push", "refs/heads/master", "someone/synarchy"),
                  ("workflow_dispatch", "refs/heads/master", "coghex/synarchy"))


class MasterTests(unittest.TestCase):
    """The cross-identity job: `master` names the identity of master's tip.

    Each test drives whole runs (publish, then reconcile) the way the
    workflow does. Reconciles that the lock serializes are simply called
    in sequence; the interleaving tests inject one run INSIDE another's
    reconcile, which the lock prevents, to show the result still converges
    on the newest plan."""

    def setUp(self):
        self.fake = FakeRegistry()
        self.fake.repo(deps.REPOSITORY)["public"] = True
        self.docker = FakeDocker(self.fake)
        self.git = FakeGit(["c1"], plans={"c1": PLAN_A})
        self.resolved: list[str] = []
        self.logs: list[str] = []

    def tearDown(self):
        self.fake.close()

    def resolve_tip(self, commit):
        self.resolved.append(commit)
        return tag_of(self.git.plan_of(commit))

    def publish(self, commit):
        identity = deps.plan_identity(self.git.plan_of(commit), OS_2404)
        return deps.publish(registry=deps.Registry(self.fake.url,
                                                   credentials=self.fake.credentials),
                            anonymous=deps.Registry(self.fake.url), docker=self.docker,
                            event="push", ref="refs/heads/master",
                            repository="coghex/synarchy", tag=identity.tag,
                            plan_sha256=identity.plan_sha256,
                            base_image="ubuntu@sha256:" + "1" * 64, revision=commit,
                            build_url="https://example/run", log=self.logs.append)

    def reconcile(self, commit, registry=None, event="push", ref="refs/heads/master",
                  repository="coghex/synarchy"):
        return deps.reconcile_master(
            registry=registry or deps.Registry(self.fake.url,
                                               credentials=self.fake.credentials),
            anonymous=deps.Registry(self.fake.url), git=self.git,
            resolve_tip=self.resolve_tip, event=event, ref=ref, repository=repository,
            tag=tag_of(self.git.plan_of(commit)), revision=commit, log=self.logs.append)

    def full_run(self, commit):
        """A whole run of `commit`: the publish job, then the reconcile."""
        self.publish(commit)
        return self.reconcile(commit)

    def master(self):
        return self.fake.digest_of(deps.REPOSITORY, deps.MOVING_TAG)

    def digest(self, plan):
        return self.fake.digest_of(deps.REPOSITORY, tag_of(plan))

    def master_history(self):
        return [digest for tag, digest in self.fake.manifest_puts if tag == deps.MOVING_TAG]

    # -- single runs ---------------------------------------------------------
    def test_new_identity_moves_master(self):
        result = self.full_run("c1")
        self.assertTrue(result.moved)
        self.assertEqual(self.master(), self.digest(PLAN_A))
        self.assertEqual(result.target, tag_of(PLAN_A))
        self.assertEqual(len(result.anonymous), 1)
        self.assertEqual(self.resolved, [])

    def test_unchanged_identity_writes_nothing(self):
        self.full_run("c1")
        self.git.push("c2")
        before = len(self.fake.writes())
        result = self.full_run("c2")
        self.assertFalse(result.moved)
        self.assertEqual(len(self.fake.writes()), before)
        self.assertEqual(len(self.docker.builds), 1)

    def test_rollback_reuses_a_and_moves_master_back(self):
        self.full_run("c1")
        self.git.push("c2", PLAN_B)
        self.full_run("c2")
        self.assertEqual(self.master(), self.digest(PLAN_B))
        self.git.push("c3", PLAN_A)
        again = self.full_run("c3")
        self.assertTrue(again.moved)
        self.assertEqual(self.master(), self.digest(PLAN_A))
        self.assertEqual(len(self.docker.builds), 2)

    def test_code_and_docs_only_commits_after_use_own_identity(self):
        self.git = FakeGit(["c1", "code1", "docs1"], plans={"c1": PLAN_A})
        result = self.full_run("c1")
        self.assertTrue(result.moved)
        self.assertIn("no plan input changed", result.reason)
        self.assertEqual(self.resolved, [])

    def test_edited_inputs_with_the_same_plan_resolve_to_own_identity(self):
        self.git = FakeGit(["c1", "e1"], plans={"c1": PLAN_A}, edits={"e1"})
        result = self.full_run("c1")
        self.assertEqual(self.resolved, ["e1"])
        self.assertTrue(result.moved)
        self.assertEqual(self.master(), self.digest(PLAN_A))

    def test_rewritten_master_follows_the_new_tip(self):
        self.git = FakeGit(["c1"], plans={"c1": PLAN_A})
        self.publish("c1")
        self.git = FakeGit(["x1"], plans={"x1": PLAN_B})
        self.publish("x1")
        self.git.plans["c1"] = PLAN_A
        result = deps.reconcile_master(
            registry=deps.Registry(self.fake.url, credentials=self.fake.credentials),
            anonymous=deps.Registry(self.fake.url), git=self.git,
            resolve_tip=self.resolve_tip, event="push", ref="refs/heads/master",
            repository="coghex/synarchy", tag=tag_of(PLAN_A), revision="c1",
            log=self.logs.append)
        self.assertEqual(self.resolved, ["x1"])
        self.assertEqual(result.target, tag_of(PLAN_B))
        self.assertEqual(self.master(), self.digest(PLAN_B))

    def test_retry_recovers_a_failed_moving_tag_update(self):
        self.publish("c1")
        self.fake.fault("PUT", r"/manifests/master$", 500)
        with self.assertRaises(deps.RegistryError):
            self.reconcile("c1")
        self.assertIsNone(self.master())
        retry = self.reconcile("c1")
        self.assertTrue(retry.moved)
        self.assertEqual(self.master(), self.digest(PLAN_A))

    def test_unpublished_own_identity_is_refused(self):
        with self.assertRaisesRegex(deps.DepsImageError, "not published"):
            self.reconcile("c1")
        self.assertEqual(self.master_history(), [])

    def test_failed_tip_resolution_writes_nothing(self):
        self.publish("c1")
        self.git.push("c2", PLAN_B)

        def broken(commit):
            raise deps.DepsImageError("docker build failed (1)")

        self.resolve_tip = broken
        with self.assertRaisesRegex(deps.DepsImageError, "docker build"):
            self.reconcile("c1")
        self.assertEqual(self.master_history(), [])

    def test_refused_events_write_nothing(self):
        self.publish("c1")
        self.fake.requests.clear()
        for event, ref, repository in REFUSED_EVENTS:
            with self.assertRaises(deps.Refused, msg=event):
                self.reconcile("c1", event=event, ref=ref, repository=repository)
        self.assertEqual(self.fake.requests, [])

    # -- runs the lock serializes ---------------------------------------------
    def test_older_reconcile_after_a_newer_one_keeps_the_newer_plan(self):
        self.publish("c1")
        self.git.push("c2", PLAN_B)
        self.full_run("c2")
        older = self.reconcile("c1")
        self.assertFalse(older.moved)
        self.assertEqual(older.target, tag_of(PLAN_B))
        self.assertEqual(self.master(), self.digest(PLAN_B))
        self.assertNotIn(self.digest(PLAN_A), self.master_history())

    def test_older_reconcile_before_the_newer_plan_is_published_leaves_master(self):
        self.full_run("c1")
        first = self.master()
        self.git.push("c2", PLAN_B)
        self.git.push("c3")
        older = self.reconcile("c1")
        self.assertFalse(older.moved)
        self.assertIn("not published yet", older.reason)
        self.assertEqual(self.master(), first)
        newer = self.full_run("c3")
        self.assertTrue(newer.moved)
        self.assertEqual(self.master(), self.digest(PLAN_B))

    def test_a_cancelled_newer_reconcile_is_done_by_the_one_that_replaced_it(self):
        # c2's reconcile was pending when c1's (older, slower) queued, so
        # GitHub cancelled it; c1's runs instead and must still reach B.
        self.publish("c1")
        self.git.push("c2", PLAN_B)
        self.publish("c2")
        older = self.reconcile("c1")
        self.assertTrue(older.moved)
        self.assertEqual(self.resolved, ["c2"])
        self.assertEqual(self.master(), self.digest(PLAN_B))
        self.assertEqual(self.master_history(), [self.digest(PLAN_B)])

    # -- interleavings the lock rules out; the result still converges --------
    def test_a_checks_b_publishes_and_moves_a_puts_ends_on_b(self):
        # The review's race: A decides while c1 is the tip, B lands, publishes
        # and moves master, then A writes. A's re-check sees the new tip and
        # repairs master to B's identity.
        self.publish("c1")
        registry = HookedRegistry(self.fake.url, credentials=self.fake.credentials)

        def b_lands_and_finishes():
            self.git.push("c2", PLAN_B)
            self.full_run("c2")
            self.assertEqual(self.master(), self.digest(PLAN_B))

        registry.before_put = b_lands_and_finishes
        a = self.reconcile("c1", registry=registry)
        self.assertEqual(self.master(), self.digest(PLAN_B))
        self.assertEqual(a.target, tag_of(PLAN_B))
        self.assertIn("advanced", " ".join(a.notes))
        self.assertEqual(self.master_history()[-1], self.digest(PLAN_B))

    def test_mirror_b_checks_a_reconciles_b_puts_ends_on_b(self):
        # The mirror: newer B decides, older A's whole reconcile runs before
        # B's write. A targets the tip (B), not its own plan, so master never
        # names A once B is published.
        self.publish("c1")
        self.git.push("c2", PLAN_B)
        self.publish("c2")
        registry = HookedRegistry(self.fake.url, credentials=self.fake.credentials)
        inner = []
        registry.before_put = lambda: inner.append(self.reconcile("c1"))
        b = self.reconcile("c2", registry=registry)
        self.assertEqual(inner[0].target, tag_of(PLAN_B))
        self.assertEqual(b.target, tag_of(PLAN_B))
        self.assertEqual(self.master(), self.digest(PLAN_B))
        self.assertNotIn(self.digest(PLAN_A), self.master_history())

    def test_a_push_during_the_decision_is_repaired_by_the_re_check(self):
        # B lands (and publishes) right after A read the tip: A writes its
        # own plan, re-reads the tip, and repairs master to B.
        self.publish("c1")

        def b_lands():
            self.git.push("c2", PLAN_B)
            self.publish("c2")

        self.git.on_tip[2] = b_lands
        a = self.reconcile("c1")
        self.assertEqual(self.master_history(), [self.digest(PLAN_A), self.digest(PLAN_B)])
        self.assertEqual(self.master(), self.digest(PLAN_B))
        self.assertTrue(a.moved)

    def test_master_that_keeps_advancing_stops_after_the_round_limit(self):
        self.publish("c1")
        # Each round reads the tip twice (decide, re-check); a new plan lands
        # on every re-check, so every round finds master advanced again.
        plans = iter([PLAN_B, PLAN_C, PLAN_A, PLAN_B])
        for round_ in range(1, deps.RECONCILE_ROUNDS + 1):
            def lands(round_=round_, plan=next(plans)):
                self.git.push(f"n{round_}", plan)
                self.publish(f"n{round_}")
            self.git.on_tip[2 * round_] = lands
        a = self.reconcile("c1")
        self.assertIn("kept advancing", " ".join(a.notes))
        self.assertEqual(len(self.master_history()), deps.RECONCILE_ROUNDS)
        # The newest push's own reconcile (queued behind the lock) finishes it.
        newest = self.reconcile(self.git.history[-1])
        self.assertEqual(newest.target, tag_of(self.git.plan_of(self.git.history[-1])))
        self.assertEqual(self.master(), self.digest(self.git.plan_of(self.git.history[-1])))


class RealGitTests(unittest.TestCase):
    """`Git`'s plan-input diff and tip worktree against real git, in a
    scratch repository: which paths count as plan inputs decides whether a
    reconcile may use its own identity for master's tip."""

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.repo = Path(self.tmp.name) / "repo"
        self.repo.mkdir()
        self.git("init", "-q", "-b", "master")
        self.write("cabal.project", "packages: .\n")
        self.write("synarchy.cabal", "name: synarchy\n")
        self.write(".github/deps/Dockerfile", "FROM scratch\n")
        self.write("docs/a.md", "a\n")
        self.write("src/A.hs", "module A where\n")
        self.base = self.commit("base")

    def tearDown(self):
        self.tmp.cleanup()

    def git(self, *args):
        return subprocess.run(["git", "-C", str(self.repo), "-c", "user.name=t",
                               "-c", "user.email=t@t", *args], check=True,
                              capture_output=True, text=True).stdout.strip()

    def write(self, rel, text):
        path = self.repo / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text)

    def commit(self, message):
        self.git("add", "-A")
        self.git("commit", "-q", "-m", message)
        return self.git("rev-parse", "HEAD")

    def test_which_paths_are_plan_inputs(self):
        git = deps.Git(str(self.repo))
        cases = (("docs/a.md", False), ("src/A.hs", False), ("lib/sub.cabal", False),
                 ("synarchy.cabal", True), ("cabal.project", True),
                 ("cabal.project.freeze", True), (".github/deps/Dockerfile", True))
        for rel, expected in cases:
            before = self.git("rev-parse", "HEAD")
            self.write(rel, f"changed {rel}\n")
            after = self.commit(rel)
            self.assertEqual(git.plan_inputs_changed(before, after), expected, rel)
        self.assertTrue(git.plan_inputs_changed(self.base, after))
        self.assertTrue(git.plan_inputs_changed(self.base, "0" * 40), "an error counts as a change")

    def test_tip_worktree_is_added_and_removed(self):
        git = deps.Git(str(self.repo))
        self.write("synarchy.cabal", "name: synarchy\nversion: 2\n")
        tip = self.commit("tip")
        tree = Path(self.tmp.name) / "tree"
        git.worktree(self.base, str(tree))
        self.assertEqual((tree / "synarchy.cabal").read_text(), "name: synarchy\n")
        git.remove_worktree(str(tree))
        self.assertFalse(tree.exists())
        self.assertNotIn(str(tree), self.git("worktree", "list"))
        with self.assertRaises(deps.DepsImageError):
            git.worktree("0" * 40, str(tree))
        self.assertEqual(tip, self.git("rev-parse", "HEAD"))


# ---------------------------------------------------------------------------
# Docker-free consumption

class FetchTests(unittest.TestCase):
    def setUp(self):
        self.fake = FakeRegistry()
        self.tmp = tempfile.TemporaryDirectory()
        self.root = Path(self.tmp.name) / "root"
        self.root.mkdir()

    def tearDown(self):
        self.fake.close()
        self.tmp.cleanup()

    def publish(self, layers, public=True):
        return self.fake.put_image(deps.REPOSITORY, "master", layers, public=public)

    def fetch(self, **kwargs):
        return deps.fetch(deps.Registry(self.fake.url), "master", self.root,
                          log=lambda _line: None, **kwargs)

    def test_layers_apply_in_order_within_the_prefixes(self):
        base = make_layer([
            ("etc/passwd", "file", "host must not get this", 0o644),
            ("usr/bin/tool", "file", "nor this", 0o755),
            ("usr/local/.ghcup/", "dir", None, 0o755),
            ("usr/local/.ghcup/bin/ghc", "file", "#!/bin/sh\n", 0o755),
            ("usr/local/.ghcup/old.txt", "file", "old", 0o644),
            ("usr/local/.ghcup/link", "symlink", "bin/ghc", 0o777),
            ("usr/local/.ghcup/hard", "hardlink", "usr/local/.ghcup/bin/ghc", 0o755),
            ("usr/local/cabal/config", "file", "v1", 0o644),
            ("usr/local/cabal/dir/a", "file", "a", 0o644),
            ("usr/local/cabal/dir/b", "file", "b", 0o644)])
        upper = make_layer([
            ("usr/local/.ghcup/.wh.old.txt", "file", "", 0o644),
            ("usr/local/cabal/dir/.wh..wh..opq", "file", "", 0o644),
            ("usr/local/cabal/dir/c", "file", "c", 0o600),
            ("usr/local/cabal/config", "file", "v2", 0o644)])
        self.publish([base, upper])
        self.fetch()
        ghcup, cabal = self.root / "usr/local/.ghcup", self.root / "usr/local/cabal"
        self.assertFalse((self.root / "etc").exists())
        self.assertFalse((self.root / "usr/bin").exists())
        self.assertFalse((ghcup / "old.txt").exists())
        self.assertEqual(sorted(p.name for p in (cabal / "dir").iterdir()), ["c"])
        self.assertEqual((cabal / "config").read_text(), "v2")
        self.assertEqual(os.readlink(ghcup / "link"), "bin/ghc")
        self.assertEqual((ghcup / "hard").stat().st_ino, (ghcup / "bin/ghc").stat().st_ino)
        self.assertEqual((ghcup / "bin/ghc").stat().st_mode & 0o777, 0o755)
        self.assertEqual((cabal / "dir/c").stat().st_mode & 0o777, 0o600)
        self.assertEqual(int((cabal / "config").stat().st_mtime), 1_700_000_000)

    def test_whiteout_of_a_whole_prefix(self):
        self.publish([make_layer([("usr/local/.ghcup/bin/ghc", "file", "x", 0o755)]),
                      make_layer([("usr/local/.wh..ghcup", "file", "", 0o644),
                                  ("usr/local/cabal/x", "file", "y", 0o644)])])
        self.fetch()
        self.assertFalse((self.root / "usr/local/.ghcup").exists())
        self.assertTrue((self.root / "usr/local/cabal/x").exists())

    def test_escapes_are_refused(self):
        outside = Path(self.tmp.name) / "outside"
        outside.mkdir()
        cases = (
            [("usr/local/cabal/../../../evil", "file", "x", 0o644)],
            [("usr/local/cabal/esc", "symlink", str(outside), 0o777),
             ("usr/local/cabal/esc/pwn", "file", "x", 0o644)],
            [("usr/local/cabal/h", "hardlink", "etc/shadow", 0o644)],
            [("usr/local/cabal/fifo", "fifo", None, 0o644)])
        for entries in cases:
            self.publish([make_layer(entries)])
            with self.assertRaises(deps.DepsImageError, msg=entries[-1][0]):
                self.fetch(allow_existing=True)
        self.assertEqual(list(outside.iterdir()), [])

    def test_directory_entry_never_follows_a_symlink_out(self):
        outside = Path(self.tmp.name) / "outside"
        outside.mkdir(mode=0o755)
        self.publish([make_layer([("usr/local/cabal/d", "symlink", str(outside), 0o777)]),
                      make_layer([("usr/local/cabal/d", "dir", None, 0o700)])])
        self.fetch()
        self.assertEqual(outside.stat().st_mode & 0o777, 0o755)
        self.assertFalse((self.root / "usr/local/cabal/d").is_symlink())
        self.assertEqual((self.root / "usr/local/cabal/d").stat().st_mode & 0o777, 0o700)

    def test_confinement_accepts_the_real_root(self):
        # `fetch --root /` is the documented default; every test above uses
        # a temporary root, so pin the `/` case of the confinement check.
        self.assertTrue(deps.is_within("/", "/usr/local"))
        self.assertTrue(deps.is_within("/", "/"))
        self.assertTrue(deps.is_within("/tmp/r", "/tmp/r/usr"))
        self.assertTrue(deps.is_within("/tmp/r", "/tmp/r"))
        self.assertFalse(deps.is_within("/tmp/r", "/tmp/rx"))
        self.assertFalse(deps.is_within("/tmp/r", "/etc"))

    # -- confinement to the prefixes (#2874 re-review) ------------------------
    # The outside target is a SIBLING of the prefixes inside the temporary
    # root (`usr/local/victim`), so it is within the root: exactly where the
    # old root-containment check, which is vacuous for the default root `/`,
    # accepted it.

    def victim(self):
        victim = self.root / "usr/local/victim"
        (victim / "sub").mkdir(parents=True)
        (victim / "keep").write_text("keep")
        (victim / "sub" / "keep").write_text("keep")
        (victim / "secret").write_text("secret")
        (victim / "d").mkdir(mode=0o755)
        return victim

    def snapshot(self, directory):
        return sorted((str(p.relative_to(directory)), p.is_dir(),
                       p.stat().st_mode & 0o777) for p in directory.rglob("*"))

    def assert_refused(self, layers, victim, needle="symlink"):
        before = self.snapshot(victim)
        self.publish(layers)
        with self.assertRaisesRegex(deps.DepsImageError, needle):
            self.fetch()
        self.assertEqual(self.snapshot(victim), before, "the outside directory changed")

    ESCAPE = make_layer([("usr/local/cabal/esc", "symlink", "../victim", 0o777)])

    def test_write_through_a_prefix_symlink_to_a_sibling_is_refused(self):
        victim = self.victim()
        for target in ("../victim", None):
            link = make_layer([("usr/local/cabal/esc", "symlink",
                                target or str(victim), 0o777)])
            self.assert_refused([link, make_layer([
                ("usr/local/cabal/esc/pwn", "file", "x", 0o644)])], victim)
            shutil.rmtree(self.root / "usr/local/cabal")
        self.assertFalse((victim / "pwn").exists())

    def test_a_refused_entry_creates_no_directory_through_the_symlink(self):
        victim = self.victim()
        self.assert_refused([self.ESCAPE, make_layer([
            ("usr/local/cabal/esc/newdir/deeper/file", "file", "x", 0o644)])], victim)
        self.assertFalse((victim / "newdir").exists())

    def test_whiteout_through_a_prefix_symlink_is_refused(self):
        victim = self.victim()
        self.assert_refused([self.ESCAPE, make_layer([
            ("usr/local/cabal/esc/.wh.keep", "file", "", 0o644)])], victim)
        self.assertTrue((victim / "keep").exists())

    def test_opaque_whiteout_through_a_prefix_symlink_is_refused(self):
        victim = self.victim()
        self.assert_refused([self.ESCAPE, make_layer([
            ("usr/local/cabal/esc/sub/.wh..wh..opq", "file", "", 0o644)])], victim)
        self.assertTrue((victim / "sub" / "keep").exists())

    def test_hard_link_source_through_a_prefix_symlink_is_refused(self):
        victim = self.victim()
        self.assert_refused([self.ESCAPE, make_layer([
            ("usr/local/cabal/h", "hardlink", "usr/local/cabal/esc/secret", 0o644)])],
            victim)
        self.assertFalse((self.root / "usr/local/cabal/h").exists())

    def test_directory_mode_never_follows_a_later_symlink_above_it(self):
        victim = self.victim()
        self.assert_refused([make_layer([
            ("usr/local/cabal/x", "dir", None, 0o755),
            ("usr/local/cabal/x/d", "dir", None, 0o700),
            ("usr/local/cabal/x", "symlink", "../victim", 0o777)])], victim,
            "never followed|replaced")
        self.assertEqual((victim / "d").stat().st_mode & 0o777, 0o755)

    def test_symlink_inside_a_prefix_is_never_followed_even_to_the_prefix(self):
        self.publish([make_layer([("usr/local/cabal/real", "dir", None, 0o755),
                                  ("usr/local/cabal/alias", "symlink", "real", 0o777)]),
                      make_layer([("usr/local/cabal/alias/f", "file", "x", 0o644)])])
        with self.assertRaisesRegex(deps.DepsImageError, "never followed"):
            self.fetch()
        self.assertFalse((self.root / "usr/local/cabal/real/f").exists())

    def test_a_prefix_root_delivered_as_a_symlink_is_refused(self):
        victim = self.victim()
        for kind, payload in (("symlink", "victim"), ("file", "x")):
            self.assert_refused([make_layer([
                ("usr/local/cabal", kind, payload, 0o777),
                ("usr/local/cabal/x", "file", "x", 0o644)])], victim, "prefix root")
            self.assertFalse(os.path.lexists(self.root / "usr/local/cabal"))

    def test_a_host_prefix_that_is_a_symlink_is_refused_for_every_branch(self):
        # Whiteouts naming the prefix's ancestors (opaque and plain) and a
        # plain entry: none may act through a prefix that redirects.
        victim = self.victim()
        (self.root / "usr/local/cabal").symlink_to("victim")
        for entries in ([("usr/local/.wh..wh..opq", "file", "", 0o644)],
                        [("usr/.wh.local", "file", "", 0o644)],
                        [("usr/local/.wh.cabal", "file", "", 0o644)],
                        [("usr/local/cabal/keep", "file", "overwritten", 0o644)]):
            self.publish([make_layer(entries)])
            with self.assertRaisesRegex(deps.DepsImageError, "symlink", msg=entries[0][0]):
                self.fetch(allow_existing=True)
            self.assertEqual((victim / "keep").read_text(), "keep", entries[0][0])
            self.assertTrue((self.root / "usr/local/cabal").is_symlink())

    def test_ancestor_whiteouts_still_clear_real_prefixes(self):
        self.publish([make_layer([("usr/local/cabal/a", "file", "a", 0o644),
                                  ("usr/local/.ghcup/b", "file", "b", 0o644)]),
                      make_layer([("usr/local/.wh..wh..opq", "file", "", 0o644)])])
        self.fetch()
        self.assertEqual(list((self.root / "usr/local/cabal").iterdir()), [])
        self.assertEqual(list((self.root / "usr/local/.ghcup").iterdir()), [])
        self.publish([make_layer([("usr/local/cabal/a", "file", "a", 0o644)]),
                      make_layer([("usr/.wh.local", "file", "", 0o644)])])
        self.fetch(allow_existing=True)
        self.assertFalse(os.path.lexists(self.root / "usr/local/cabal"))

    def test_containment_is_to_the_resolved_prefixes_not_the_root(self):
        # The check itself, with the default root `/` (read-only: resolving
        # paths writes nothing): every absolute path is within `/`, so only
        # the resolved prefixes can confine.
        real = deps.resolved_prefixes(Path("/")).values()
        self.assertTrue(deps.is_within("/", "/tmp/elsewhere"))
        for path, inside in (("/usr/local/cabal", True), ("/usr/local/cabal/store/x", True),
                             ("/usr/local/.ghcup/bin", True), ("/usr/local", False),
                             ("/usr/local/cabalx", False), ("/usr/local/victim", False),
                             ("/tmp/elsewhere", False), ("/etc/passwd", False), ("/", False)):
            self.assertEqual(deps.within_prefixes(real, path), inside, path)

    def test_a_host_ancestor_redirected_after_the_prefix_is_open_changes_nothing(self):
        # The prefix is opened once and everything is resolved from that
        # descriptor, so a host directory above it swapped for a symlink
        # afterwards cannot redirect a later write.
        elsewhere = self.root / "elsewhere"
        (elsewhere / "cabal").mkdir(parents=True)
        with deps.Jail(self.root) as jail:
            self.assertIsNotNone(jail.prefix_fd("usr/local/cabal", True))
            real = self.root / "usr/local"
            real.rename(self.root / "usr/local-moved")
            real.symlink_to(elsewhere)
            parent, name = jail.parent_of(("usr", "local", "cabal", "x"), True)
            os.close(os.open(name, os.O_WRONLY | os.O_CREAT, 0o644, dir_fd=parent))
            os.close(parent)
        self.assertEqual(list((elsewhere / "cabal").iterdir()), [])
        self.assertTrue((self.root / "usr/local-moved/cabal/x").exists())

    def test_tampered_blob_fails_its_digest(self):
        layer = make_layer([("usr/local/cabal/x", "file", "x", 0o644)])
        self.publish([layer])
        repo = self.fake.repos[deps.REPOSITORY]
        middle = len(layer) // 2
        repo["blobs"][sha(layer)] = layer[:middle] + bytes([layer[middle] ^ 0xFF]) + layer[middle + 1:]
        with self.assertRaisesRegex(deps.RegistryError, "digest"):
            self.fetch()

    def test_refuses_to_unpack_over_existing_prefixes(self):
        self.publish([make_layer([("usr/local/cabal/x", "file", "x", 0o644)])])
        (self.root / "usr/local/cabal").mkdir(parents=True)
        (self.root / "usr/local/cabal/mine").write_text("keep")
        with self.assertRaisesRegex(deps.DepsImageError, "empty prefixes"):
            self.fetch()
        self.assertEqual((self.root / "usr/local/cabal/mine").read_text(), "keep")

    def test_private_image_is_not_anonymous(self):
        self.publish([make_layer([("usr/local/cabal/x", "file", "x", 0o644)])], public=False)
        with self.assertRaises(deps.NotAnonymous):
            self.fetch()

    def test_anonymous_check_covers_both_tags(self):
        digest = self.publish([make_layer([("usr/local/cabal/x", "file", "x", 0o644)])])
        self.fake.repos[deps.REPOSITORY]["manifests"]["ubuntu24.04-x"] = \
            self.fake.repos[deps.REPOSITORY]["manifests"]["master"]
        report = deps.verify_anonymous(deps.Registry(self.fake.url), ["ubuntu24.04-x", "master"])
        self.assertEqual(len(report), 2)
        self.assertTrue(all(digest in line for line in report))
        heads = [path for method, path in self.fake.requests if method == "HEAD"]
        self.assertEqual(len(heads), 4)


# ---------------------------------------------------------------------------
# Workflow and Dockerfile wiring

WORKFLOW = REPO / ".github" / "workflows" / "deps-image.yml"
DOCKERFILE = REPO / ".github" / "deps" / "Dockerfile"
CI_DOCKERFILE = REPO / ".github" / "ci" / "Dockerfile"


def apt_lists(text: str) -> list[list[str]]:
    lists = []
    for block in re.findall(r"apt-get install -y --no-install-recommends\s*\\\n(.*?)(?:&&|\n\n)",
                            text, re.S):
        lists.append(sorted(word for word in re.findall(r"^\s+([a-z0-9][a-z0-9.+-]*)\s*\\?$",
                                                        block, re.M)))
    return lists


class HostileArchiveTests(unittest.TestCase):
    """Whatever a layer names, nothing outside the two prefixes is created,
    modified, replaced or deleted (#2874: the third tar-extraction
    confinement finding, so the class, not one case). The fixture puts
    canaries where every escape lands: siblings of the prefixes inside the
    root, the root's parent, and a directory outside the root reached by an
    absolute path."""

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.scratch = tempfile.TemporaryDirectory()      # the layers, apart from the fixture
        self.base = Path(self.tmp.name)
        self.root = self.base / "root"
        self.outside = self.base / "outside"
        self.build_fixture()

    def tearDown(self):
        self.tmp.cleanup()
        self.scratch.cleanup()

    def build_fixture(self):
        for directory in (self.root / "usr/local/victim/sub", self.root / "usr/local/cabalx",
                          self.root / "etc", self.root / "usr/bin", self.outside / "sub"):
            directory.mkdir(parents=True)
        for path in (self.root / "usr/local/victim/keep", self.root / "usr/local/victim/sub/keep",
                     self.root / "usr/local/cabalx/keep", self.root / "etc/passwd",
                     self.root / "usr/bin/tool", self.outside / "keep",
                     self.outside / "sub/keep", self.base / "sibling"):
            path.write_text("keep")
        os.link(self.root / "etc/passwd", self.root / "etc/passwd-hard")
        (self.root / "usr/local/victim/link").symlink_to("keep")
        (self.root / "usr/local/victim/sub").chmod(0o755)

    def outside_state(self):
        """Everything not inside a prefix: type, mode, content, mtime, link
        count and target. The two host directories above the prefixes are
        compared by mode only (creating a prefix legitimately touches their
        times)."""
        prefixes = (self.root / "usr/local/cabal", self.root / "usr/local/.ghcup")
        state = {}
        for path in sorted(self.base.rglob("*")):
            if any(path == prefix or prefix in path.parents for prefix in prefixes):
                continue
            info = path.lstat()
            entry = [stat.S_IFMT(info.st_mode), info.st_mode & 0o7777]
            if path in (self.root / "usr", self.root / "usr/local"):
                entry.append(sorted(c.name for c in path.iterdir()
                                    if c.name not in ("cabal", ".ghcup")))
            else:
                entry += [info.st_nlink, int(info.st_mtime)]
                if stat.S_ISREG(info.st_mode):
                    entry.append(path.read_bytes())
                elif stat.S_ISLNK(info.st_mode):
                    entry.append(os.readlink(path))
            state[str(path.relative_to(self.base))] = entry
        return state

    def apply(self, *layers):
        """Apply layers in order; a refusal is fine, anything else is not."""
        for index, layer in enumerate(layers):
            path = Path(self.scratch.name) / f"layer{index}.tar.gz"
            path.write_bytes(layer)
            try:
                deps.apply_layer(path, self.root)
            except deps.DepsImageError:
                break

    def assert_contained(self, *layers, msg=""):
        before = self.outside_state()
        self.apply(*layers)
        after = self.outside_state()
        self.assertEqual(after, before, msg or "something outside the prefixes changed")

    def test_the_decoded_whiteout_traversal_the_reviewer_reproduced(self):
        self.assert_contained(make_layer([("usr/local/cabal/.wh...", "file", "", 0o644)]))
        self.assertTrue((self.root / "usr/local/victim/keep").exists())

    def test_whiteouts_of_dot_dotdot_and_empty_are_refused(self):
        # with the prefixes present, where a decoded `..` would reach `usr/local`
        path = Path(self.scratch.name) / "base.tar.gz"
        path.write_bytes(make_layer([("usr/local/cabal/a", "file", "a", 0o644),
                                     ("usr/local/.ghcup/b", "file", "b", 0o644)]))
        deps.apply_layer(path, self.root)
        for name in ("usr/local/cabal/.wh..", "usr/local/cabal/.wh...", "usr/local/cabal/.wh.",
                     "usr/local/.wh...", "usr/.wh..", ".wh.."):
            with self.subTest(name=name):
                path = Path(self.scratch.name) / "layer.tar.gz"
                path.write_bytes(make_layer([(name, "file", "", 0o644)]))
                with self.assertRaises(deps.DepsImageError):
                    deps.apply_layer(path, self.root)
                self.assertTrue((self.root / "usr/local/victim/keep").exists())
                self.assertTrue((self.root / "usr/local/victim/sub/keep").exists())

    def test_hard_link_targets_are_decoded_like_every_other_name(self):
        victim = str(self.root / "usr/local/victim/keep")
        for source in ("usr/local/cabal/../victim/keep", "/usr/local/victim/keep", victim,
                       "usr/local/cabal//x", "usr/local/cabal/./x", "usr/local/cabal/..", "../x"):
            with self.subTest(source=source):
                self.assert_contained(make_layer([
                    ("usr/local/cabal/h", "hardlink", source, 0o644)]), msg=source)
                self.assertFalse(os.path.lexists(self.root / "usr/local/cabal/h"))

    def test_nested_symlink_chains_are_never_followed(self):
        outside = str(self.outside)
        for chain in ((("l1", "l2"), ("l2", "../victim")),
                      (("l1", "l2"), ("l2", "l3"), ("l3", outside)),
                      (("l1", "."), ("l2", "l1/.."), ("l3", "l2/../../victim"))):
            entries = [(f"usr/local/cabal/{name}", "symlink", target, 0o777)
                       for name, target in chain]
            for tail in ("l1/pwn", "l1/sub/pwn", "l1/.wh.keep", "l1/sub/.wh..wh..opq",
                         "l1/d"):
                kind = "dir" if tail.endswith("/d") or tail == "l1/d" else "file"
                with self.subTest(chain=chain, tail=tail):
                    self.assert_contained(make_layer(entries), make_layer([
                        (f"usr/local/cabal/{tail}", kind, "x", 0o755)]))
                    shutil.rmtree(self.root / "usr/local/cabal", ignore_errors=True)

    def test_absolute_and_dotted_entry_names_are_refused(self):
        for name in ("/usr/local/cabal/x", "/etc/passwd", "usr/local/cabal/./../x",
                     "usr/local//cabal/x", "usr/local/cabal/x/../../../victim/keep"):
            with self.subTest(name=name):
                self.assert_contained(make_layer([(name, "file", "pwn", 0o644)]), msg=name)

    def test_duplicate_entries_with_different_types(self):
        self.assert_contained(make_layer([
            ("usr/local/cabal/x", "dir", None, 0o755),
            ("usr/local/cabal/x/y", "file", "1", 0o644),
            ("usr/local/cabal/x", "symlink", "../victim", 0o777),
            ("usr/local/cabal/x/keep", "file", "pwn", 0o644),
            ("usr/local/cabal/x", "dir", None, 0o700),
            ("usr/local/cabal/x", "file", "f", 0o644)]))

    # -- the property -----------------------------------------------------
    NAMES = ("..", ".", "", "esc", "victim", "cabal", ".ghcup", "a", "x", "keep", "sub", "etc",
             ".wh.", ".wh..", ".wh...", ".wh.keep", ".wh.sub", ".wh.esc", ".wh.victim",
             ".wh..wh..opq", ".wh..wh.plnk", ".wh.cabal", ".wh.local", "l1", "l2")
    ROOTS = ("usr/local/cabal", "usr/local/.ghcup", "usr/local", "usr", "", ".",
             "./usr/local/cabal", "/usr/local/cabal", "usr/local/cabalx", "usr/local/victim")
    TARGETS = ("../victim", "../../victim", "../../../outside", "..", ".", "/", "l1", "l2", "a",
               "esc", "x/../..", "usr/local/victim", "usr/local/victim/keep",
               "usr/local/cabal/a", "usr/local/cabal/l1", "etc/passwd", "usr/bin/tool",
               "/etc/passwd", "usr/local/cabal/../victim/keep", "usr/local/cabal", "")

    def entry(self, rng):
        parts = [rng.choice(self.ROOTS)] + [rng.choice(self.NAMES)
                                            for _ in range(rng.randint(0, 4))]
        name = "/".join(part for part in parts if part != "" or rng.random() < 0.15)
        if rng.random() < 0.35:
            # a whiteout or opaque marker whose DECODED victim may be `.`, `..` or empty
            name += "/.wh." + rng.choice(("..", ".", "", "..wh..opq", "keep", "esc", "x"))
        if rng.random() < 0.15:
            name += "/"
        target = rng.choice(self.TARGETS + (str(self.outside), str(self.root / "usr/local/victim"),
                                            str(self.root / "usr/local/victim/keep")))
        kind = rng.choice(("file", "file", "dir", "dir", "symlink", "symlink", "hardlink",
                           "hardlink", "fifo"))
        if kind == "hardlink" and rng.random() < 0.5:
            target = rng.choice(self.ROOTS) + "/" + rng.choice(self.NAMES)
        return (name, kind, "x" if kind == "file" else target, rng.choice((0o644, 0o755, 0o700, 0o777)))

    def test_no_hostile_archive_touches_anything_outside_the_prefixes(self):
        count = int(os.environ.get("HOSTILE_ARCHIVE_CASES", "600"))
        for seed in range(count):
            rng = random.Random(seed)
            # the prefixes exist (half the time): a hostile name can only reach
            # outside through a prefix that is there to be addressed
            layers = ([make_layer([("usr/local/cabal/", "dir", None, 0o755),
                                   ("usr/local/cabal/a", "file", "a", 0o644),
                                   ("usr/local/.ghcup/", "dir", None, 0o755)])]
                      if rng.random() < 0.5 else [])
            for _ in range(rng.randint(1, 3)):
                entries = [self.entry(rng) for _ in range(rng.randint(2, 9))]
                # a chain of links to a planted name makes later entries hostile
                if rng.random() < 0.6:
                    entries.insert(0, ("usr/local/cabal/esc", "symlink",
                                       rng.choice(self.TARGETS), 0o777))
                try:
                    layers.append(make_layer(entries))
                except (ValueError, tarfile.TarError):
                    continue
            self.tearDown()
            self.setUp()
            before = self.outside_state()
            self.apply(*layers)
            self.assertEqual(self.outside_state(), before, f"seed {seed}")


class WiringTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        import yaml
        cls.workflow = yaml.safe_load(WORKFLOW.read_text())
        from ci_parity_workflow import job_would_run
        cls.job_would_run = staticmethod(job_would_run)

    def test_only_master_pushes_trigger_it(self):
        triggers = self.workflow.get(True) or self.workflow.get("on")
        self.assertEqual(triggers, {"push": {"branches": ["master"]}})

    def test_jobs_run_only_for_master_pushes_of_this_repository(self):
        base = {"github.event_name": "push", "github.ref": "refs/heads/master",
                "github.repository": "coghex/synarchy",
                "needs.deps-plan.outputs.tag": "ubuntu24.04-x86_64-ghc9.12.2-ab",
                "ancestors_succeeded": True, "cancelled": False}
        scenarios = (
            ("a master push", {}, True),
            ("a same-repository pull request", {"github.event_name": "pull_request",
                                                "github.ref": "refs/pull/1/merge"}, False),
            ("a fork pull request", {"github.event_name": "pull_request",
                                     "github.ref": "refs/pull/2/merge",
                                     "github.repository": "coghex/synarchy"}, False),
            ("pull_request_target", {"github.event_name": "pull_request_target"}, False),
            ("a push to another branch", {"github.ref": "refs/heads/other"}, False),
            ("a fork's own master push", {"github.repository": "fork/synarchy"}, False),
            ("a cancelled run", {"cancelled": True}, False))
        for job in ("deps-plan", "deps-image", "deps-master"):
            condition = self.workflow["jobs"][job]["if"]
            for label, override, expected in scenarios:
                context = dict(base, **override)
                self.assertEqual(self.job_would_run(condition, context), expected,
                                 f"{job} on {label}")
        for job in ("deps-image", "deps-master"):
            condition = self.workflow["jobs"][job]["if"]
            self.assertFalse(self.job_would_run(condition, dict(
                base, **{"needs.deps-plan.outputs.tag": ""})), f"{job} on a docs-only push")
            # No status function: GitHub's implicit success() over the needed
            # jobs, so a failed publish never reaches the master job.
            self.assertFalse(self.job_would_run(condition, dict(
                base, ancestors_succeeded=False)), f"{job} after a failed need")

    def test_lock_permissions_and_commands(self):
        jobs = self.workflow["jobs"]
        image = jobs["deps-image"]
        self.assertEqual(image["concurrency"], {
            "group": "synarchy-deps-${{ needs.deps-plan.outputs.tag }}",
            "cancel-in-progress": False})
        self.assertEqual(self.workflow["permissions"], {"contents": "read"})
        self.assertEqual(jobs["deps-plan"]["permissions"], {"contents": "read"})
        self.assertEqual(image["permissions"], {"contents": "read", "packages": "write"})
        for job in jobs.values():
            self.assertEqual(job["runs-on"], "ubuntu-24.04")
        plan_run = " ".join(s.get("run", "") for s in jobs["deps-plan"]["steps"])
        self.assertIn("python3 tools/ci_deps_image.py plan", plan_run)
        self.assertIn("python3 tools/ci_docs_fast_path.py --stdin", plan_run)
        publish = " ".join(s.get("run", "") for s in image["steps"])
        for fragment in ("python3 tools/ci_deps_image.py publish",
                         "--event '${{ github.event_name }}'", "--ref '${{ github.ref }}'",
                         "--repository '${{ github.repository }}'", "--sha '${{ github.sha }}'"):
            self.assertIn(fragment, publish)

    def test_master_is_written_only_under_one_lock_for_every_identity(self):
        # The review's race (#2874): per-identity groups do not serialize two
        # identities, so the moving tag needs a lock of its own, FIXED (no
        # expression), with no cancellation of a running holder, and held by
        # the only job that writes it.
        jobs = self.workflow["jobs"]
        master = jobs["deps-master"]
        self.assertEqual(master["concurrency"], {
            "group": deps.MASTER_LOCK_GROUP, "cancel-in-progress": False})
        self.assertNotIn("${{", master["concurrency"]["group"])
        self.assertNotEqual(master["concurrency"]["group"],
                            jobs["deps-image"]["concurrency"]["group"])
        self.assertEqual(set(master["needs"]), {"deps-plan", "deps-image"})
        self.assertEqual(master["permissions"], {"contents": "read", "packages": "write"})
        checkout = master["steps"][0]
        self.assertTrue(checkout["uses"].startswith("actions/checkout@"))
        self.assertEqual(checkout.get("with"), {"fetch-depth": 0},
                         "the decision needs master's history and a tip worktree")
        runs = {name: " ".join(s.get("run", "") for s in job["steps"])
                for name, job in jobs.items()}
        writers = [name for name, text in runs.items() if "reconcile-master" in text]
        self.assertEqual(writers, ["deps-master"])
        self.assertNotIn("ci_deps_image.py publish", runs["deps-master"])
        for fragment in ("python3 tools/ci_deps_image.py reconcile-master",
                         "--tag '${{ needs.deps-plan.outputs.tag }}'",
                         "--base-image '${{ needs.deps-plan.outputs.base_image }}'",
                         "--event '${{ github.event_name }}'", "--ref '${{ github.ref }}'",
                         "--repository '${{ github.repository }}'", "--sha '${{ github.sha }}'"):
            self.assertIn(fragment, runs["deps-master"])

    def test_dockerfile_matches_the_ci_image_and_the_repository(self):
        mine, ci = DOCKERFILE.read_text(), CI_DOCKERFILE.read_text()
        self.assertEqual(len(apt_lists(mine)), 1)
        self.assertEqual(apt_lists(mine)[0], apt_lists(ci)[-1],
                         "apt build dependencies differ from .github/ci/Dockerfile")
        for arg in ("GHC_VERSION", "CABAL_VERSION"):
            pattern = rf"^ARG {arg}=(\S+)$"
            self.assertEqual(re.search(pattern, mine, re.M).group(1),
                             re.search(pattern, ci, re.M).group(1), arg)
        self.assertIn("FROM ${BASE_IMAGE} AS toolchain", mine)
        self.assertIn("ARG BASE_IMAGE=ubuntu:24.04", mine)
        self.assertIn('rm -rf "$G/share/doc"', mine)
        self.assertIn(r"find \"$G\" \( -name '*.p_hi' -o -name '*.p_o' -o -name '*_p.a' \) -delete",
                      mine.replace('"$G"', '\\"$G\\"'))
        self.assertIn(f"ENV CABAL_DIR={deps.CABAL_DIR}", mine)
        self.assertIn(f"WORKDIR {deps.WORKSPACE}", mine)
        copied = re.search(r"^COPY (cabal\.project.*?) \./$", mine, re.M).group(1).split()
        tracked = subprocess.run(["git", "-C", str(REPO), "ls-files"], capture_output=True,
                                 text=True).stdout.split() or [p.name for p in REPO.iterdir()]
        inputs = sorted(p for p in tracked if "/" not in p
                        and (p.startswith("cabal.project") or p.endswith(".cabal")))
        self.assertEqual(sorted(copied), inputs)


if __name__ == "__main__":
    unittest.main(verbosity=1)
