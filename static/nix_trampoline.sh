#!/usr/bin/env bash

ARGV0=$1
shift 1
exec -a "$ARGV0" nix-static "$@"
