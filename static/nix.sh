#!/usr/bin/env sh

exec nah nix_trampoline.sh "$(basename $0)" "$@"
