#!/usr/bin/env python3
"""Manual map-page codec and fine-cache experiment (#2303).

Full protocol and interpretation: docs/world_map_page_codec_measurement.md.
The self-test imports the pure model only; measurement is never a CI gate.
"""
import argparse
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import platform
import statistics
import subprocess
import sys
import tempfile

import map_page_codec_bridge as bridge
import map_page_codec_model as model

CORPUS = bridge.ROOT / "tools/map_codec/corpus.json"


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def measured_page(exe, directory, ident, codec):
    rgba_path = directory / f"{ident}.rgba"
    pristine = rgba_path.read_bytes()
    encoded = directory / f"{ident}.{codec}"
    first = bridge.run(exe, ["encode", codec, rgba_path, encoded])
    second_path = directory / f"{ident}.{codec}.repeat"
    second = bridge.run(exe, ["encode", codec, rgba_path, second_path])
    identical = encoded.read_bytes() == second_path.read_bytes()
    decodes, baselines, round_trips = [], [], []
    decoded = directory / f"{ident}.{codec}.decoded"
    for _ in range(3):
        baselines.append(bridge.run(exe, ["baseline", codec, encoded, decoded]))
        sample = bridge.run(exe, ["decode", codec, encoded, decoded])
        decodes.append(sample)
        round_trips.append(sample["ok"] and decoded.read_bytes() == pristine)
    encoded_bytes = encoded.read_bytes()
    corruptions = []
    for offset in model.corruption_offsets(len(encoded_bytes)):
        corrupt = directory / f"{ident}.{codec}.flip-{offset}"
        changed = bytearray(encoded_bytes)
        changed[offset] ^= 1
        corrupt.write_bytes(changed)
        corrupt.with_name(corrupt.name + ".sha256").write_bytes(
            encoded.with_name(encoded.name + ".sha256").read_bytes())
        checked = bridge.run(exe, ["decode", codec, corrupt, decoded])
        native = bridge.run(exe, ["native", codec, corrupt, decoded])
        corruptions.append({"offset": offset, "xor": 1,
                            "external_integrity_ok": checked["ok"],
                            "native_decoder_ok": native["ok"],
                            "native_reason": native.get("reason"),
                            **model.damage(pristine, decoded.read_bytes() if native["ok"] else None)})
    peak = statistics.median(d["peak_process_rss_bytes"] for d in decodes)
    baseline = statistics.median(b["peak_process_rss_bytes"] for b in baselines)
    return {"id": ident, "codec": codec,
            "payload_encoded_bytes": len(encoded_bytes),
            "external_checksum_bytes": 32,
            "encoded_bytes": len(encoded_bytes) + 32,
            "ratio": float(model.ratio(len(encoded_bytes) + 32)),
            "rgba_sha256": digest(rgba_path), "encoded_sha256": digest(encoded),
            "in_process_identical": first["in_process_identical"] and second["in_process_identical"],
            "fresh_process_identical": identical,
            "deterministic": first["in_process_identical"] and second["in_process_identical"] and identical,
            "round_trip": all(round_trips),
            "encode_samples_ns": first["nanoseconds"],
            "encode_median_ns": statistics.median(first["nanoseconds"]),
            "isolated_decode_samples": decodes,
            "isolated_decode_median_ns": statistics.median(d["nanoseconds"] for d in decodes),
            "baseline_samples": baselines,
            "peak_process_rss_median_bytes": peak,
            "baseline_peak_rss_median_bytes": baseline,
            "peak_rss_increment_bytes": max(0, peak - baseline),
            "corruptions": corruptions,
            "corruption_detected": all(not c["external_integrity_ok"] for c in corruptions)}


def quota_models(rows, inventories, quotas=(16, 64, 256, 1024), codecs=model.CODECS):
    # Verify the Python address arithmetic against the actual WML-5
    # inventory before using it for any synthetic camera requests.
    for inv in inventories:
        for level in inv["levels"]:
            if model.level_shape(inv["size"], level["level"]) != (level["u"], level["v"]):
                raise ValueError("Python trace geometry disagrees with WML-5")
    result = []
    aux = [(0, u, v) for _ in range(6) for v in range(9) for u in range(5)]
    for codec in codecs:
        measured_sizes = [r["encoded_bytes"] for r in rows if r["codec"] == codec]
        for sizing in ("median", "maximum"):
            # Unmeasured pages are priced at a DECLARED assumption,
            # with maximum-corpus-size sensitivity reported separately.
            assumed_bytes = int(statistics.median(measured_sizes)) if sizing == "median" else max(measured_sizes)
            for kind in ("home-expeditions", "frontier", "distant-inspection"):
                requests = model.camera_trace(kind)
                sizes = dict.fromkeys(requests, assumed_bytes)
                aux_sizes = dict.fromkeys(aux, assumed_bytes)
                for mib in quotas:
                    total = mib * 1024 ** 2
                    aux_quota = total // 20
                    main = model.lru(requests, sizes, total - aux_quota)
                    auxiliary = model.lru(aux, aux_sizes, aux_quota)
                    result.append({"codec": codec, "sizing_assumption": sizing,
                        "assumed_bytes_per_unmeasured_page": assumed_bytes,
                        "trace": kind, "total_quota_bytes": total,
                        "main_quota_bytes": total - aux_quota, "aux_quota_bytes": aux_quota,
                        "main": main, "auxiliary": auxiliary,
                        "combined_hit_rate": (main["hits"] + auxiliary["hits"]) /
                            (main["requests"] + auxiliary["requests"])})
    return result


def compare_run(previous, directory, rows):
    mismatches = []
    for row in rows:
        for extension in ("rgba", row["codec"], row["codec"] + ".sha256"):
            name = f"{row['id']}.{extension}"
            if (previous / name).read_bytes() != (directory / name).read_bytes():
                mismatches.append(name)
    return sorted(set(mismatches))


def aggregate(rows):
    result = {}
    for codec in model.CODECS:
        pages = [r for r in rows if r["codec"] == codec]
        total = sum(r["encoded_bytes"] for r in pages)
        result[codec] = {
            "pages": len(pages), "encoded_bytes_total": total,
            "decoded_bytes_total": len(pages) * model.DECODED_BYTES,
            "aggregate_ratio": float(model.ratio(total, len(pages) * model.DECODED_BYTES)),
            "encoded_bytes_median": statistics.median(r["encoded_bytes"] for r in pages),
            "encoded_bytes_min": min(r["encoded_bytes"] for r in pages),
            "encoded_bytes_max": max(r["encoded_bytes"] for r in pages),
            "encode_medians_sum_ns": sum(r["encode_median_ns"] for r in pages),
            "isolated_decode_medians_sum_ns": sum(r["isolated_decode_median_ns"] for r in pages),
            "isolated_decode_page_median_ns": statistics.median(r["isolated_decode_median_ns"] for r in pages),
            "peak_process_rss_max_bytes": max(r["peak_process_rss_median_bytes"] for r in pages),
        }
    return result


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--corpus-only", action="store_true")
    parser.add_argument("--output", type=Path, help="new empty output directory; default fresh system temp directory")
    parser.add_argument("--compare", type=Path, help="previous retained full run for direct-byte reproducibility")
    parser.add_argument("--replay-cache", type=Path, help="derive expanded PNG quota models from retained results; no build or codec measurement")
    args = parser.parse_args(argv)
    if args.replay_cache and (args.corpus_only or args.compare):
        parser.error("--replay-cache cannot combine with --corpus-only or --compare")
    worlds = json.loads(CORPUS.read_text())
    model.validate_corpus(worlds)
    directory = args.output.resolve() if args.output else Path(tempfile.mkdtemp(prefix="map-codec-2303-"))
    if args.output:
        directory.mkdir(parents=True, exist_ok=False)
    print(f"Retained experiment: {directory}", flush=True)
    if args.replay_cache:
        source = json.loads(args.replay_cache.read_text())
        if source["schema"] != "map-page-codec-measurement/v1" or not all(
                c["passed"] for c in model.compare_checks(source["rows"]).values()):
            raise ValueError("cache replay requires a passing codec measurement")
        replay = {"schema": "map-page-cache-replay/v1",
                  "source_sha256": digest(args.replay_cache),
                  "command": sys.argv,
                  "tool_source_sha256": {p.name: digest(p) for p in
                      (Path(__file__), Path(model.__file__))},
                  "interpretation": "All three traces are critical; no combined score selects a quota.",
                  "quota_models": quota_models(source["rows"], source["inventories"],
                      (16, 64, 256, 512, 1024, 2048, 3072, 4096), ("png",))}
        (directory / "results.json").write_text(json.dumps(replay, indent=2) + "\n")
        print(f"Derived cache replay: {directory / 'results.json'}", flush=True)
        return 0
    exe = bridge.build()
    try:
        generation = bridge.run(exe, ["generate", CORPUS, directory], parse=False, capabilities=4)
    except (RuntimeError, TimeoutError) as error:
        (directory / "generation-failure.log").write_text(str(error) + "\n")
        raise
    (directory / "generation.log").write_text(generation["stderr"])
    observed = {r["id"]: r for r in json.loads((directory / "corpus.json").read_text())}
    failures = []
    for world in worlds:
        for page in world["pages"]:
            failures += [f"{page['id']}: missing {category}"
                         for category in model.verify_observation(page, observed[page["id"]])]
    if failures:
        (directory / "fixture-failures.json").write_text(json.dumps(failures, indent=2))
        raise ValueError("corpus fixture failure: " + "; ".join(failures))
    print(f"Verified {len(observed)} corpus pages and their claimed categories", flush=True)
    if args.corpus_only:
        return 0
    rows = []
    for ident in observed:
        for codec in model.CODECS:
            print(f"Measuring {ident}: {codec}", flush=True)
            rows.append(measured_page(exe, directory, ident, codec))
            (directory / "partial-results.json").write_text(json.dumps(rows, indent=2))
    inventories = bridge.run(exe, ["inventory"])
    bulk = {}
    for codec in model.CODECS:
        manifest = directory / f"bulk-{codec}.json"
        manifest.write_text(json.dumps([str(directory / f"{ident}.{codec}") for ident in observed]))
        bulk[codec] = bridge.run(exe, ["bulk", codec, manifest, 3])
    package_plan = json.loads((bridge.ROOT / "dist-newstyle/cache/plan.json").read_text())
    packages = sorted({(p["pkg-name"], p["pkg-version"]) for p in package_plan["install-plan"]})
    report = {"schema": "map-page-codec-measurement/v1",
              "date_utc": datetime.now(timezone.utc).isoformat(),
              "platform": platform.platform(), "machine": platform.machine(),
              "python": platform.python_version(),
              "logical_cpu_count": os.cpu_count(),
              "generation_capabilities": 4, "measurement_capabilities": 1,
              "load_average_at_report": os.getloadavg(),
              "ghc": subprocess.check_output(["ghc", "--numeric-version"], text=True).strip(),
              "git_head": subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=bridge.ROOT, text=True).strip(),
              "command": sys.argv, "corpus_sha256": digest(CORPUS),
              "tool_source_sha256": {str(p.relative_to(bridge.ROOT)): digest(p)
                  for p in sorted((bridge.ROOT / "tools/map_codec").glob("*.hs")) +
                  sorted((bridge.ROOT / "tools").glob("map_page_codec*.py"))},
              "packages": packages, "inventories": inventories,
              "rows": rows, "checks": model.compare_checks(rows), "aggregate": aggregate(rows),
              "bulk": bulk, "quota_models": quota_models(rows, inventories)}
    if args.compare:
        report["previous_run_byte_mismatches"] = compare_run(args.compare.resolve(), directory, rows)
    (directory / "results.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report["checks"], indent=2))
    print(f"Results: {directory / 'results.json'}", flush=True)
    return 0 if all(c["passed"] for c in report["checks"].values()) and not report.get("previous_run_byte_mismatches") else 1


if __name__ == "__main__":
    raise SystemExit(main())
