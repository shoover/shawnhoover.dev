#!/bin/bash

# Scripts to test bootstrapping emacs binaries cache via nix.

function start_container () {
    docker run -it -v `pwd`:/mnt/data ubuntu
}

function container_1_bootstrap () {
    apt update
    apt upgrade -y
    apt install -y curl rsync sudo tree xz-utils

    USER=nixuser
    groupadd --gid 1000 $USER
    useradd --system --uid 1000 --gid $USER --groups sudo --create-home --home-dir /home/$USER --shell /bin/bash $USER
    echo "$USER ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers
    su - $USER

    curl -Lo nix.tar.gz https://github.com/purcell/setup-emacs/archive/refs/tags/v6.0.tar.gz
    tar xf nix.tar.gz
    INPUT_VERSION=29.1 GITHUB_PATH=/tmp/github_path GITHUB_ENV=/tmp/github_env setup-emacs-6.0/install.sh
    # . $HOME/.nix-profile/etc/profile.d/nix.sh
    PATH="$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:$PATH"
    nix-store --query --requisites $(which emacs) | xargs -I {} rsync -av {} /mnt/data/nix_store/
}

function container_2_restore () {
    apt update
    apt install -y rsync
    mkdir -p /nix/store
    rsync -av /mnt/data/nix_store/ /nix/store/
    emacs_store=$(find /nix/store -name "*-emacs-29-1")
    test -x $emacs_store/bin/emacs
    $emacs_store/bin/emacs --version
}
