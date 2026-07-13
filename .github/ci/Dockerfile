# This file's content hash IS the image tag (#784): .github/workflows/
# ci.yml's resolve-image job tags every build `ci-<sha256 of this file,
# 16 hex chars>` and publishes a given tag at most once — the first time
# its content is seen. There's no version number to bump anywhere:
# editing this file is the entire "refresh the CI image" procedure, and
# the content-derived tag is what makes a published tag safe to treat as
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
    tar \
    xorg-dev \
    xz-utils \
    zlib1g-dev \
 && locale-gen en_US.UTF-8 \
 && rm -rf /var/lib/apt/lists/*

ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

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
