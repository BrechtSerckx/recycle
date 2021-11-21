#! /usr/bin/env bash
# shellcheck disable=SC3030,SC3024,SC3054

GENERATE_ICS_ARGS=(generate-ics)
GENERATE_ICS_ARGS+=(--language NL)
GENERATE_ICS_ARGS+=(--relative-from 7)
GENERATE_ICS_ARGS+=(--relative-to 60)
# GENERATE_ICS_ARGS+=(--absolute-from 2021-10-13)
# GENERATE_ICS_ARGS+=(--absolute-to 2021-12-08)

API_CLIENT_ARGS=(client)
# API_CLIENT_ARGS+=(get-access-token)
API_CLIENT_ARGS+=(search-zipcodes)
API_CLIENT_ARGS+=(--access-token eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpYXQiOjE2MzQ2MzE0NzksImV4cCI6MTYzNDYzNTA3OSwiYXVkIjoicmVjeWNsZWFwcC5iZSJ9.RBcnYf-0kwpGDWvcHFO2SXYDm94doruaZrudl2h0sfA)
API_CLIENT_ARGS+=(3000)

# cabal run exe:recycle -- "${GENERATE_ICS_ARGS[@]}" "$@"
# cabal run exe:recycle -- "${API_CLIENT_ARGS[@]}" "$@"
