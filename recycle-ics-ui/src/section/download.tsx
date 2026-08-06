import * as React from "react";
import { useWatch } from "react-hook-form";
import { FormInputs, inputsToForm, Form, formToParams } from "../types";
import { serverUrl, nodeEnv } from "../env";
import { apiFetch, friendlyError } from "../api";

type PreflightState = "idle" | "checking" | "ok" | "error";

export default function DownloadSection() {
  const formInputs = useWatch() as FormInputs,
    mForm = inputsToForm(formInputs);

  const mkHttpLink = (form: Form): URL => {
      var url = new URL("/api/generate", serverUrl);
      try {
        url.search = new URLSearchParams(formToParams(form)).toString();
      } catch (error) {
        console.error(error);
      }
      return url;
    },
    mkWebcalLink = (form: Form): string => {
      return mkHttpLink(form).href.replace(/^https?:/, "webcal:");
    },
    filename = "recycle.ics";

  const httpUrl = mForm ? mkHttpLink(mForm).toString() : null;

  const [preflight, setPreflight] = React.useState<PreflightState>("idle");
  const [preflightError, setPreflightError] = React.useState<string | null>(null);

  React.useEffect(() => {
    if (!httpUrl) {
      setPreflight("idle");
      setPreflightError(null);
      return;
    }
    setPreflight("checking");
    setPreflightError(null);
    const timer = setTimeout(() => {
      apiFetch(httpUrl)
        .then(() => setPreflight("ok"))
        .catch((e: unknown) => {
          setPreflight("error");
          setPreflightError(friendlyError(e));
        });
    }, 500);
    return () => clearTimeout(timer);
  }, [httpUrl]);

  const canDownload = preflight === "ok";

  return (
    <div className="card">
      <p className="card-label">Download</p>
      {mForm ? (
        <>
          <p className="card-description">
            Use the webcal link to subscribe — your calendar will stay up to
            date automatically.
          </p>
          <textarea
            className="url-display"
            readOnly
            value={mkWebcalLink(mForm)}
          />
          {preflight === "checking" && (
            <p className="msg-loading">Verifying…</p>
          )}
          {preflight === "error" && preflightError && (
            <p className="msg-error">{preflightError}</p>
          )}
          <div className="download-actions">
            <a
              className={`btn btn-primary${canDownload ? "" : " btn-disabled"}`}
              href={canDownload ? mkWebcalLink(mForm) : undefined}
              aria-disabled={!canDownload}
            >
              Subscribe (webcal)
            </a>
            <a
              className={`btn${canDownload ? "" : " btn-disabled"}`}
              download={canDownload ? filename : undefined}
              href={canDownload ? mkHttpLink(mForm).toString() : undefined}
              aria-disabled={!canDownload}
            >
              Download .ics
            </a>
          </div>
        </>
      ) : (
        <p className="msg-empty">
          Fill in your address above to generate a link.
        </p>
      )}
      {nodeEnv === "development" && (
        <details style={{ marginTop: "1rem" }}>
          <summary>Debug: raw form state</summary>
          <pre style={{ fontSize: "0.75rem", overflowX: "auto" }}>
            {JSON.stringify(formInputs, null, 2)}
          </pre>
          <pre style={{ fontSize: "0.75rem", overflowX: "auto" }}>
            {JSON.stringify(mForm, null, 2)}
          </pre>
        </details>
      )}
    </div>
  );
}
