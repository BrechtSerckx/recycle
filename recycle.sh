#! /usr/bin/env bash
# shellcheck disable=SC3030,SC3024,SC3054

GENERATE_ARGS=(generate)
GENERATE_ARGS+=(--language NL)
GENERATE_ARGS+=(--relative-from 7)
GENERATE_ARGS+=(--relative-to 60)
# GENERATE_ARGS+=(--absolute-from 2021-10-13)
# GENERATE_ARGS+=(--absolute-to 2021-12-08)

cabal run exe:recycle -- "${GENERATE_ARGS[@]}"
