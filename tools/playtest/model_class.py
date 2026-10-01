"""Which models the playtest tools run on: the owner's Class B.

The player, the critic and the persona-flavor generator name a model class,
never a model. `modelclass` resolves the class from the owner's master file
(~/.config/model-classes.toml) when a run starts, so moving Class B to a new
model needs no change here. Every run records the model and effort it actually
used (meta.json, the critic report), so traces stay attributable.

Self-tests never resolve: they inject explicit models.
"""
from __future__ import annotations

import shutil
import subprocess

PLAYTEST_CLASS = "B"


class ModelClassError(RuntimeError):
    """The class could not be resolved into a model and effort."""


def resolve(brand: str, model_class: str = PLAYTEST_CLASS) -> tuple[str, str]:
    """(model, effort) for `brand` ("claude" or "codex") in `model_class`."""
    exe = shutil.which("modelclass")
    if exe is None:
        raise ModelClassError(
            f"the playtest tools run on Class {model_class}, resolved by "
            "`modelclass`, which is not on PATH; install it, or pass an "
            "explicit --model/--effort where the tool accepts one")
    try:
        proc = subprocess.run([exe, "get", model_class, brand],
                              text=True, capture_output=True, timeout=10)
    except subprocess.TimeoutExpired as e:
        raise ModelClassError("modelclass timed out") from e
    if proc.returncode != 0:
        raise ModelClassError(
            f"modelclass could not resolve Class {model_class} for {brand}: "
            f"{(proc.stderr or proc.stdout).strip()}")
    parts = proc.stdout.split()
    if len(parts) != 2:
        raise ModelClassError(f"unexpected modelclass output: {proc.stdout!r}")
    return parts[0], parts[1]
