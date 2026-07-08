FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
    bash \
    ca-certificates \
    curl \
    git \
    glslang-tools \
    gzip \
    libgl1-mesa-dev \
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
 && rm -rf /var/lib/apt/lists/*
