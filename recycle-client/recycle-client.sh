#! /usr/bin/env bash
# shellcheck disable=SC3030,SC3024,SC3054

SECRET="<REDACTED>"

# get access token
# CMD=(
#     get-access-token
# )
ACCESS_TOKEN="<REDACTED>"

# search a zip code
# CMD=(
#     search-zipcodes \
#     --access-token "$ACCESS_TOKEN" \
#     3000
# )
ZIP_CODE="1234-56789"

# search a street
# CMD=(
#     search-streets \
#     --access-token "$ACCESS_TOKEN"
#     --zipcode "$ZIP_CODE"
#     Grote
# )
STREET="https://data.vlaanderen.be/id/straatnaam-12345"

HOUSE_NUMBER=1

# get the fractions for an address
# CMD=(
#     get-fractions \
#     --zipcode "$ZIP_CODE" \
#     --street "$STREET" \
#     --house-number "$HOUSE_NUMBER" \
# )

# get the collections for an address
CMD=(
    get-collections \
    --zipcode "$ZIP_CODE" \
    --street "$STREET" \
    --house-number "$HOUSE_NUMBER" \
    )
# use absolute date
# CMD+=(--absolute-from 2022-01-01 --absolute-to 2022-12-31)
# use relative date
CMD+=(--relative-from -14 --relative-to 14)

# run it
cabal run exe:recycle-client -- \
      --secret "$SECRET" \
      "${CMD[@]}" \
      "$@"
