#! /usr/bin/env bash
# shellcheck disable=SC3030,SC3024,SC3054

CMD=(cabal run exe:recycle-ics --)
# CMD=(docker run -p "$RECYCLE_ICS_PORT:$RECYCLE_ICS_PORT" recycle:latest)
# CMD=($(nix-build -A recycle-ics)/bin/recycle-ics)

# Generate ICS file
ZIP_CODE="1234-56789"
STREET="https://data.vlaanderen.be/id/straatnaam-12345"
HOUSE_NUMBER=1
# CMD+=(
#     generate-ics \
#     --secret "$RECYCLE_ICS_SECRET" \
#     --zipcode "$ZIP_CODE" \
#     --street "$STREET" \
#     --house-number "$HOUSE_NUMBER" \
# )
# use absolute date
# CMD+=(--absolute-from 2022-01-01 --absolute-to 2022-12-31)
# use relative date
# CMD+=(--relative-from -14 --relative-to 14)


# servce ics files
CMD+=(serve-ics)

# run it
"${CMD[@]}" "$@"
