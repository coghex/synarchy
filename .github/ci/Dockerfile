FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive
ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

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
    pkg-config \
    python3 \
    tar \
    xorg-dev \
    xz-utils \
    zlib1g-dev \
 && rm -rf /var/lib/apt/lists/*
