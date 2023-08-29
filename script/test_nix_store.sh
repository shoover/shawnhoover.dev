#!/bin/bash

# Scripts to test bootstrapping emacs binaries cache via nix.

function start_container () {
    docker run -it -v `pwd`:/mnt/data ubuntu
}

function container_1_bootstrap () {
    apt update
    apt upgrade -y
    apt install -y curl xz-utils rsync
    curl -o setup_emacs_install https://raw.githubusercontent.com/purcell/setup-emacs/master/install.sh
    chmod u+x setup_emacs_install
    mkdir -m 0755 /nix && chown root /nix
    INPUT_VERSION=29.1 USER=root GITHUB_PATH=/tmp/github_path GITHUB_ENV=/tmp/github_env ./setup_emacs_install
    PATH="$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:$PATH"
    nix-store --query --requisites $(which emacs) | xargs -I {} rsync -av {} /mnt/data/nix_store/
}

function container_2_restore () {
    apt update
    apt install -y rsync
    mkdir -p /nix/store
    rsync -av /mnt/data/nix_store/ /nix/store/
    emacs --version
}
