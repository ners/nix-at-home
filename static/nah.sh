#!/usr/bin/env bash

if [ -d /nix ]; then
    if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then
        . $HOME/.nix-profile/etc/profile.d/nix.sh
    fi
    exec "$@"
fi
if [ -z "$NAH_NIX_ROOT" ]; then NAH_NIX_ROOT=__NAH_NIX_ROOT_DEFAULT__; fi
if [ ! -d "$NAH_NIX_ROOT/var/nix" ]; then mkdir -p "$NAH_NIX_ROOT/var/nix"; fi

exec bwrap \
    --unshare-all \
    --uid `id -u` \
    --gid `id -g` \
    --unsetenv LC_ALL \
    --setenv HOME $HOME \
    --setenv LANG $LANG \
    --setenv TERM $TERM \
    --setenv USER $USER \
    --setenv SHELL $SHELL \
    --setenv PATH $PATH \
    --setenv NIX_CACHE_HOME /tmp \
    --share-net \
    --proc /proc \
    --dev-bind /dev /dev \
    --dev-bind /sys/devices /sys/devices \
    --dev-bind /sys/class/net /sys/class/net \
    --dev-bind /run /run \
    --bind $PWD $PWD \
    --bind $HOME $HOME \
    --bind /tmp /tmp \
    --ro-bind /bin /bin \
    --ro-bind /sbin /sbin \
    --ro-bind /usr/bin /usr/bin \
    --ro-bind /lib /lib \
    --ro-bind /lib64 /lib64 \
    --ro-bind /etc /etc \
    --bind "$NAH_NIX_ROOT" /nix \
    --remount-ro / \
    "$0" "$@"
