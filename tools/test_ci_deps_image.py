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
  absence; an older run never moves `master` past a newer non-docs commit,
  while docs-only commits after it do not stop it; pull requests (fork
  and same-repository), other refs and other repositories write nothing.
* Anonymous access: both tags' manifests and blobs, and the private case.
* Docker-free consumption: layer order, whiteouts and opaque directories,
  links, prefix confinement, path and symlink escapes, digest checks, the
  CDN redirect without credentials, and refusing non-empty prefixes.
* Consumer compatibility: matching plans from another directory, and
  mismatching ones (a changed unit, another compiler).
* Wiring: deps-image.yml runs only for master pushes of this repository
  (checked with the parity audit's own scheduling model), keys its lock on
  the identity, grants packages: write to the publishing job alone; the
  Dockerfile's apt list, toolchain versions and plan inputs match the CI
  image and the repository.
"""
from __future__ import annotations

import hashlib
import http.server
import io
import json
import os
import re
import subprocess
import sys
import tarfile
import tempfile
import threading
import unittest
import urllib.parse
import uuid
from pathlib import Path

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
    """A linear master history; `docs` names the docs-only commits."""

    def __init__(self, history, docs=()):
        self.history = list(history)
        self.docs = set(docs)

    def master_tip(self):
        return self.history[-1]

    def is_ancestor(self, older, newer):
        return older in self.history and newer in self.history and \
            self.history.index(older) <= self.history.index(newer)

    def docs_only_between(self, older, newer):
        span = self.history[self.history.index(older) + 1:self.history.index(newer) + 1]
        return all(commit in self.docs for commit in span)


class PublishTests(unittest.TestCase):
    def setUp(self):
        self.fake = FakeRegistry()
        self.fake.repo(deps.REPOSITORY)["public"] = True
        self.docker = FakeDocker(self.fake)
        self.git = FakeGit(["c1"])
        self.logs: list[str] = []

    def tearDown(self):
        self.fake.close()

    def clients(self):
        return (deps.Registry(self.fake.url, credentials=self.fake.credentials),
                deps.Registry(self.fake.url))

    def run_publish(self, commit, plan=None, event="push", ref="refs/heads/master",
                    repository="coghex/synarchy"):
        plan_bytes = plan or make_plan()
        identity = deps.plan_identity(plan_bytes, OS_2404)
        registry, anonymous = self.clients()
        return deps.publish(registry=registry, anonymous=anonymous, docker=self.docker,
                            git=self.git, event=event, ref=ref, repository=repository,
                            tag=identity.tag, plan_sha256=identity.plan_sha256,
                            base_image="ubuntu@sha256:" + "1" * 64, revision=commit,
                            build_url="https://example/run", log=self.logs.append)

    def master(self):
        return self.fake.digest_of(deps.REPOSITORY, deps.MOVING_TAG)

    def test_new_identity_publishes_once_and_moves_master(self):
        result = self.run_publish("c1")
        self.assertTrue(result.built and result.pushed and result.moved)
        self.assertEqual(self.master(), result.digest)
        self.assertEqual(len(self.docker.builds), 1)
        self.assertEqual(self.docker.builds[0][1]["EXPECTED_PLAN_SHA256"],
                         result.tag.rsplit("-", 1)[1])
        labels = self.docker.builds[0][2]
        self.assertEqual(labels["org.opencontainers.image.revision"], "c1")
        self.assertEqual(labels["org.opencontainers.image.base.digest"], "sha256:" + "1" * 64)
        self.assertEqual(len(result.anonymous), 2)

    def test_unchanged_identity_publishes_nothing(self):
        first = self.run_publish("c1")
        self.git.history.append("c2")
        before = len(self.fake.writes())
        second = self.run_publish("c2")
        self.assertFalse(second.built or second.pushed or second.moved)
        self.assertEqual(second.digest, first.digest)
        self.assertEqual(len(self.docker.builds), 1)
        self.assertEqual(len(self.fake.writes()), before)

    def test_rollback_reuses_a_and_moves_master_back(self):
        plan_a, plan_b = make_plan(), make_plan(units=[unit("zlib", "0.7.1.2", "ddd")])
        a = self.run_publish("c1", plan_a)
        self.git.history.append("c2")
        b = self.run_publish("c2", plan_b)
        self.assertEqual(self.master(), b.digest)
        self.git.history.append("c3")
        again = self.run_publish("c3", plan_a)
        self.assertFalse(again.built)
        self.assertTrue(again.moved)
        self.assertEqual(again.digest, a.digest)
        self.assertEqual(self.master(), a.digest)
        self.assertEqual(len(self.docker.builds), 2)

    def test_concurrent_run_publishing_first_is_kept(self):
        identity = deps.plan_identity(make_plan(), OS_2404)
        layer = make_layer([("usr/local/cabal/store/other-run", "file", "x", 0o644)])
        winner = []

        def other_run_finishes_first():
            winner.append(self.fake.put_image(deps.REPOSITORY, identity.tag, [layer]))

        self.docker.on_build = other_run_finishes_first
        result = self.run_publish("c1")
        self.assertTrue(result.built)
        self.assertFalse(result.pushed)
        self.assertEqual(result.digest, winner[0])
        self.assertEqual(self.fake.digest_of(deps.REPOSITORY, identity.tag), winner[0])
        self.assertIn("kept its image", " ".join(result.notes))

    def test_retry_recovers_a_failed_moving_tag_update(self):
        self.fake.fault("PUT", r"/manifests/master$", 500)
        with self.assertRaises(deps.RegistryError):
            self.run_publish("c1")
        self.assertIsNone(self.master())
        retry = self.run_publish("c1")
        self.assertFalse(retry.built)
        self.assertTrue(retry.moved)
        self.assertEqual(self.master(), retry.digest)

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

    def test_older_run_never_moves_master_back(self):
        plan_a, plan_b = make_plan(), make_plan(units=[unit("zlib", "0.7.1.2", "ddd")])
        self.git = FakeGit(["c1", "c2"])
        newer = self.run_publish("c2", plan_a)
        older = self.run_publish("c1", plan_b)
        self.assertTrue(older.pushed)
        self.assertFalse(older.moved)
        self.assertEqual(self.master(), newer.digest)

    def test_docs_only_commits_after_do_not_block_the_move(self):
        self.git = FakeGit(["c1", "d1", "d2"], docs={"d1", "d2"})
        result = self.run_publish("c1")
        self.assertTrue(result.moved)
        self.assertIn("docs-only", result.move_reason)

    def test_rewritten_master_is_left_alone(self):
        self.git = FakeGit(["x1", "x2"])
        result = self.run_publish("c1")
        self.assertTrue(result.pushed)
        self.assertFalse(result.moved)

    def test_pull_requests_and_other_refs_write_nothing(self):
        cases = (("pull_request", "refs/pull/7/merge", "coghex/synarchy"),
                 ("pull_request_target", "refs/heads/master", "coghex/synarchy"),
                 ("push", "refs/heads/feature", "coghex/synarchy"),
                 ("push", "refs/heads/master", "someone/synarchy"),
                 ("workflow_dispatch", "refs/heads/master", "coghex/synarchy"))
        for event, ref, repository in cases:
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
        with contextlib.redirect_stderr(io.StringIO()) as err:
            code = deps.main(["publish", "--tag", "t", "--plan-sha256", "p", "--base-image", "b",
                              "--event", "pull_request", "--ref", "refs/pull/1/merge",
                              "--repository", "coghex/synarchy", "--sha", "c1",
                              "--registry-url", self.fake.url])
        self.assertIn("pull requests never do", err.getvalue())
        self.assertEqual(code, 3)
        self.assertEqual(self.fake.requests, [])


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
        for job in ("deps-plan", "deps-image"):
            condition = self.workflow["jobs"][job]["if"]
            for label, override, expected in scenarios:
                context = dict(base, **override)
                self.assertEqual(self.job_would_run(condition, context), expected,
                                 f"{job} on {label}")
        image_if = self.workflow["jobs"]["deps-image"]["if"]
        self.assertFalse(self.job_would_run(image_if, dict(
            base, **{"needs.deps-plan.outputs.tag": ""})), "docs-only push")
        self.assertFalse(self.job_would_run(image_if, dict(base, ancestors_succeeded=False)))

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
