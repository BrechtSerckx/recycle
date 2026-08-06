import { serverUrl, githubUrl } from "./env";

export type ErrorCause =
  | "service_unavailable"
  | "invalid_request"
  | "decode_error"
  | "other_error";

export class ApiError extends Error {
  constructor(message: string, public readonly cause: ErrorCause | null) {
    super(message);
    this.name = "ApiError";
  }
}

export function friendlyError(error: unknown): string {
  if (error instanceof ApiError) {
    switch (error.cause) {
      case "service_unavailable":
        return "The recycleapp.be service is temporarily unavailable. Please try again later.";
      case "decode_error":
        return `Unexpected error (this may be a bug — please report it at ${githubUrl}/issues): ${error.message}`;
      default:
        return error.message;
    }
  }
  if (error instanceof Error) return error.message;
  return String(error);
}

async function apiFetch(url: string): Promise<Response> {
  let response: Response;
  try {
    response = await fetch(url);
  } catch {
    throw new ApiError(
      "Could not connect to the server. Please check your connection.",
      null
    );
  }
  if (!response.ok) {
    let cause: ErrorCause | null = null;
    let message = `HTTP ${response.status}`;
    try {
      const body = await response.json();
      if (body.cause) cause = body.cause as ErrorCause;
      if (body.message) message = body.message;
    } catch {
      const text = await response.text().catch(() => "");
      if (text) message = text;
    }
    throw new ApiError(message, cause);
  }
  return response;
}

export function searchZipcodes(q: string): Promise<any[]> {
  return apiFetch(`${serverUrl}api/search-zipcode?q=${q}&lang_code=nl`).then(
    (r) => r.json()
  );
}

export function searchStreets(zipcode: string, q: string): Promise<any[]> {
  return apiFetch(
    `${serverUrl}api/search-street?zipcode=${zipcode}&q=${q}`
  ).then((r) => r.json());
}

export function getFractions(
  zipcode: string,
  street: string,
  houseNumber: number
): Promise<any[]> {
  return apiFetch(
    `${serverUrl}api/fractions?zipcode=${zipcode}&street=${street}&house_number=${houseNumber}`
  ).then((r) => r.json());
}
