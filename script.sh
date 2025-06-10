if [ -d /nix ]; then
    . $HOME/.nix-profile/etc/profile.d/nix.sh
    exec "$@"
fi

exec bwrap \
    --unshare-all \
    --uid `id -u` \
    --gid `id -g` \
    --clearenv \
    --setenv HOME $HOME \
    --setenv LANG $LANG \
    --setenv TERM $TERM \
    --setenv USER $USER \
    --setenv SHELL $SHELL \
    --share-net \
    --proc /proc \
    --dev /dev \
    --bind $PWD $PWD \
    --bind $HOME $HOME \
    --bind /tmp /tmp \
    --ro-bind /bin /bin \
    --ro-bind /usr/bin /usr/bin \
    --ro-bind /lib /lib \
    --ro-bind /lib64 /lib64 \
    --ro-bind $(realpath /etc/resolv.conf) /etc/resolv.conf \
    --ro-bind $(realpath /etc/ssl/certs) /etc/ssl/certs \
    --bind $HOME/nixroot /nix \
    --remount-ro / \
    "$0" "$@"
