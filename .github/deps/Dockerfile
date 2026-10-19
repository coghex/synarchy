# synarchy-deps (#2864): Ubuntu 24.04, the CI image's apt build
# dependencies, GHC + cabal through ghcup at the CI image's prefixes, the
# Hackage index snapshot, and the compiled Cabal dependency store for ONE
# resolved dependency plan. Published to ghcr.io/coghex/synarchy-deps by
# .github/workflows/deps-image.yml; consumption is documented in
# docs/deps_image.md.
#
# Deliberately NOT built on the CI image (.github/ci/Dockerfile). Its
# consumers are Ubuntu 24.04 cloud sandboxes, and Cabal's unit IDs hash
# the system pkg-config versions of `vulkan` and `zlib`, so only a store
# built against 24.04's own packages is reused there in full. Built
# natively: the workflow runs this on an ubuntu-24.04 runner, FROM the
# ubuntu:24.04 digest it resolved.
#
# Stages, in the order tools/ci_deps_image.py drives them:
#   toolchain    apt packages, ghcup GHC/cabal (no profiling libraries,
#                no bundled docs), `cabal update`.
#   plan         resolves the plan at the FIXED workspace /src/synarchy
#                with CABAL_DIR=/usr/local/cabal, so plan.json's absolute
#                paths, and therefore its hash, do not depend on the
#                runner, the run or a temporary directory.
#   plan-export  plan.json + os-release, the inputs of the identity tag.
#   store        builds every dependency of that exact plan and checks it.
#   image        toolchain + the store + its metadata file. Published.
#
# There is no tag to bump here. The image's identity is the resolved plan
# (see ci_deps_image.py), so editing this file republishes nothing until
# the plan itself changes.
ARG BASE_IMAGE=ubuntu:24.04

FROM ${BASE_IMAGE} AS toolchain
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
ENV DEBIAN_FRONTEND=noninteractive
# Exactly the build dependencies .github/ci/Dockerfile installs (its second
# `apt-get install`; tools/test_ci_deps_image.py fails if the two lists
# drift). Its git-core PPA is not needed: 24.04 ships git 2.43, newer than
# the 2.38 that PPA exists for.
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

# The CI image's prefixes, so the store keeps its CI path and a consumer
# sets the same CABAL_DIR. ghcup itself also lives under /usr/local/.ghcup,
# so the two directories a consumer unpacks are the whole toolchain.
# `cabal update`'s snapshot time is kept in $CABAL_DIR/index-state, so a
# consumer can tell whether its checkout's index-state pin is covered.
# Profiling is local-only (#2870): GHC's profiling libraries and its
# bundled documentation are removed in the same layer that installs them,
# and the build fails if any survive.
ENV GHCUP_INSTALL_BASE_PREFIX=/usr/local
ENV CABAL_DIR=/usr/local/cabal
ENV PATH=/usr/local/.ghcup/bin:${PATH}
ARG GHC_VERSION=9.12.2
ARG CABAL_VERSION=3.16.1.0
RUN mkdir -p /usr/local/.ghcup/bin \
 && curl --proto '=https' --tlsv1.2 -sSf \
      https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup \
      -o /usr/local/.ghcup/bin/ghcup \
 && chmod +x /usr/local/.ghcup/bin/ghcup \
 && ghcup config add-release-channel prereleases \
 && ghcup install ghc "${GHC_VERSION}" --set \
 && ghcup install cabal "${CABAL_VERSION}" --set \
 && rm -rf /usr/local/.ghcup/cache /usr/local/.ghcup/tmp /usr/local/.ghcup/logs \
 && G="/usr/local/.ghcup/ghc/${GHC_VERSION}" \
 && rm -rf "$G/share/doc" \
 && find "$G" \( -name '*.p_hi' -o -name '*.p_o' -o -name '*_p.a' \) -delete \
 && test ! -e "$G/share/doc" \
 && test -z "$(find "$G" \( -name '*.p_hi' -o -name '*.p_o' -o -name '*_p.a' \) -print -quit)" \
 && ghc --version && cabal --version
RUN cabal update | tee /tmp/cabal-update.log \
 && sed -n 's/^The index-state is set to \([^ ]*Z\)\.*$/\1/p' /tmp/cabal-update.log \
      | tail -n 1 > "${CABAL_DIR}/index-state" \
 && test -s "${CABAL_DIR}/index-state" \
 && rm /tmp/cabal-update.log

FROM toolchain AS plan
WORKDIR /src/synarchy
# Every tracked plan input at the repository root; the test above fails
# when a new one (a cabal.project.freeze, an imported project file)
# appears without being copied here.
COPY cabal.project synarchy.cabal ./
RUN cabal build all -v0 --dry-run

FROM scratch AS plan-export
COPY --from=plan /src/synarchy/dist-newstyle/cache/plan.json /plan.json
COPY --from=plan /usr/lib/os-release /os-release

FROM plan AS store
# The identity the plan job resolved. Builds only if this build resolved
# the same plan, so the store is bound to the tag it is published under.
# --enable-tests: the plan already includes both test suites (the solver
# enables their stanzas when it can, and plan.json is byte-identical with
# or without the flag), but `cabal build all` does not build their
# dependencies. Without it, `cabal build synarchy-test-headless` would
# still compile HUnit, hspec and friends on every consumer.
# store-metadata then requires the store to hold every unit the local
# components reach, and cabal's own dry-run to build nothing else.
ARG EXPECTED_PLAN_SHA256
ARG SOURCE_REVISION
ARG BASE_IMAGE
ARG BUILD_URL=
RUN test -n "${EXPECTED_PLAN_SHA256}" \
 && echo "${EXPECTED_PLAN_SHA256}  dist-newstyle/cache/plan.json" | sha256sum -c - \
 && cabal build all -v0 --enable-tests --only-dependencies
COPY tools/ci_deps_image.py /opt/synarchy-deps/ci_deps_image.py
RUN cabal build all --enable-tests --dry-run > /tmp/dry-run.txt \
 && python3 /opt/synarchy-deps/ci_deps_image.py store-metadata \
      --plan dist-newstyle/cache/plan.json \
      --expect-plan-sha256 "${EXPECTED_PLAN_SHA256}" \
      --dry-run /tmp/dry-run.txt \
      --os-release /usr/lib/os-release \
      --source-revision "${SOURCE_REVISION}" \
      --base-image "${BASE_IMAGE}" \
      --build-url "${BUILD_URL}" \
      --output /usr/local/cabal/synarchy-deps.json

FROM toolchain AS image
COPY --from=store /usr/local/cabal/store /usr/local/cabal/store
COPY --from=store /usr/local/cabal/synarchy-deps.json /usr/local/cabal/synarchy-deps.json
