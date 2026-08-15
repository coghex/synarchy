# This file's content hash (together with .github/workflows/ci-image.yml's
# own bytes) IS the image tag (#784): ci-image.yml's tag job computes
# `ci-<sha256 of both files, 16 hex chars>` and publishes a given tag at
# most once — the first time that exact content pair is seen. There's no
# version number to bump anywhere: editing this file (or ci-image.yml's
# build recipe) is the entire "refresh the CI image" procedure, and the
# content-derived tag is what makes a published tag safe to treat as
# immutable (a later edit always produces a new tag, never overwrites
# this one).
FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
    bash \
    build-essential \
    ca-certificates \
    curl \
    git \
    glslang-tools \
    gzip \
    libffi-dev \
    libgl1-mesa-dev \
    libgmp-dev \
    libncurses-dev \
    libvulkan-dev \
    libxcursor-dev \
    libxi-dev \
    libxinerama-dev \
    libxrandr-dev \
    libxxf86vm-dev \
    locales \
    pkg-config \
    python3 \
    python3-pip \
    tar \
    xorg-dev \
    xz-utils \
    zlib1g-dev \
 && locale-gen en_US.UTF-8 \
 && rm -rf /var/lib/apt/lists/*

ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

# The Python toolchain the unit-asset tools run on (#1257 validator,
# #1258 atlas compiler). These pins are duplicated from
# tools/requirements-assets.txt ON PURPOSE and must stay identical to
# it: the image tag is the hash of THIS file plus ci-image.yml, so a
# `COPY` of the requirements file would let a pin change silently reuse
# a stale image. tools/test_pack_atlas.py fails if the two drift, which
# is what makes the duplication safe.
#
# python3-yaml (apt) was replaced by the pip pin so exactly one PyYAML
# is importable. Pillow is the real decoder both halves of the tool
# need — the compiler to read and write atlases, and, since #1311, the
# inventory gate itself to decode every declared frame. Neither is
# optional here: without Pillow the gate reports one error and exits
# non-zero rather than skipping the content checks, so a CI image
# missing it fails loudly instead of validating nothing.
RUN python3 -m pip install --no-cache-dir \
      "PyYAML==6.0.2" \
      "Pillow==11.3.0"

# Toolchain baked into the image (#671): the ghcup-cache approach in
# ci.yml never hit once the job moved into this container (the action
# installs via the mounted runner tool cache, not /usr/local/.ghcup), so
# every run re-downloaded GHC (~2 min). Baking it here makes setup a
# no-op. Everything lives under HOME-independent prefixes on purpose:
# GitHub container jobs run steps with HOME=/github/home, so anything
# installed under the build-time HOME (/root) would be invisible at run
# time. CABAL_DIR pins cabal's config/index/store to one absolute path
# for the same reason — it also gives ci.yml a static store path to
# cache. The `cabal update` snapshots the hackage index so the per-run
# update is an incremental delta fetch, not a full index download.
ENV GHCUP_INSTALL_BASE_PREFIX=/usr/local
ENV CABAL_DIR=/usr/local/cabal
ENV PATH=/usr/local/.ghcup/bin:${PATH}
ARG GHC_VERSION=9.12.2
ARG CABAL_VERSION=3.16.1.0
RUN curl --proto '=https' --tlsv1.2 -sSf \
      https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup \
      -o /usr/local/bin/ghcup \
 && chmod +x /usr/local/bin/ghcup \
 # prereleases channel matches what haskell-actions/setup configured, so
 # the exact same cabal 3.16.1.0 release stays resolvable
 && ghcup config add-release-channel prereleases \
 && ghcup install ghc "${GHC_VERSION}" --set \
 && ghcup install cabal "${CABAL_VERSION}" --set \
 && cabal update \
 && ghc --version && cabal --version \
 && rm -rf /usr/local/.ghcup/cache /usr/local/.ghcup/tmp
