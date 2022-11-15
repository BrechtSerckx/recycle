#! /usr/bin/env bash
# shellcheck disable=SC3030,SC3024,SC3054

CMD=(cabal run exe:recycle --)
SECRET="<REDACTED>"
CMD+=("--secret" "$SECRET")

# CMD+=(generate-ics)
# CMD+=(--language NL)
# CMD+=(--relative-from 7)
# CMD+=(--relative-to 60)
# GENERATE_ICS_ARGS+=(--absolute-from 2021-10-13)
# GENERATE_ICS_ARGS+=(--absolute-to 2021-12-08)

# CMD+=(client)
# CMD+=(get-access-token)
# CMD+=(search-zipcodes)
# ACCESS_TOKEN="<REDACTED>"
# CMD+=(--access-token "$ACCESS_TOKEN")
# CMD+=(3000)

CMD+=(serve-ics)
CMD+=(--port 3333)

"${CMD[@]}" "$@"
