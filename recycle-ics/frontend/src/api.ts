const host = "http://localhost:3332";
export function searchZipcodes(q: string) {
  return fetch(`${host}/api/search-zipcode?q=${q}&lang_code=nl`).then(
    (response) => response.json()
  );
}

export function searchStreets(zipcode: string, q: string) {
  return fetch(`${host}/api/search-street?zipcode=${zipcode}&q=${q}`).then(
    (response) => response.json()
  );
}

export function getFractions(
  zipcode: string,
  street: string,
  house_number: number
) {
  return fetch(
    `${host}/api/fractions?zipcode=${zipcode}&street=${street}&house_number=${house_number}`
  ).then((response) => response.json());
}
