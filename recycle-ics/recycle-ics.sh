#! /usr/bin/env bash
# shellcheck disable=SC3030,SC3024,SC3054

PORT=3332

CMD=(cabal run exe:recycle-ics --)
# CMD=(docker run -p "$PORT:$PORT" recycle:latest)

SECRET="<REDACTED>"
CMD+=(--secret "$SECRET")

# Generate ICS file
ZIP_CODE="1234-56789"
STREET="https://data.vlaanderen.be/id/straatnaam-12345"
HOUSE_NUMBER=1
# CMD+=(
#     generate-ics \
#     --zipcode "$ZIP_CODE" \
#     --street "$STREET" \
#     --house-number "$HOUSE_NUMBER" \
# )
# use absolute date
# CMD+=(--absolute-from 2022-01-01 --absolute-to 2022-12-31)
# use relative date
# CMD+=(--relative-from -14 --relative-to 14)


# servce ics files
CMD+=(
    serve-ics \
    --port "$PORT"
)

# run it
"${CMD[@]}" "$@"
