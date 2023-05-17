import { serverUrl } from "./env";

export function searchZipcodes(q: string) {
  return fetch(`${serverUrl}api/search-zipcode?q=${q}&lang_code=nl`).then(
    (response) => response.json()
  );
}

export function searchStreets(zipcode: string, q: string) {
  return fetch(`${serverUrl}api/search-street?zipcode=${zipcode}&q=${q}`).then(
    (response) => response.json()
  );
}

export function getFractions(
  zipcode: string,
  street: string,
  houseNumber: number
) {
  return fetch(
    `${serverUrl}api/fractions?zipcode=${zipcode}&street=${street}&house_number=${houseNumber}`
  ).then((response) => response.json());
}
