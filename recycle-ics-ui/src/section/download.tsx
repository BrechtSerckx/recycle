import * as React from "react";
import { useFormContext, useWatch } from "react-hook-form";
import { FormInputs, inputsToForm, Form, formToParams } from "../types";
import { serverUrl, nodeEnv } from "../env";
import { apiFetch, friendlyError } from "../api";

type CheckState = "idle" | "checking" | "ok" | "error";

export default function DownloadSection() {
  const { formState } = useFormContext<FormInputs>();
  const formInputs = useWatch() as FormInputs,
    mForm = formState.isValid ? inputsToForm(formInputs) : null;

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

  const [checkState, setCheckState] = React.useState<CheckState>("idle");
  const [checkError, setCheckError] = React.useState<string | null>(null);

  // Any change to the generated request invalidates a previous check —
  // the user has to hit Validate again before downloading.
  React.useEffect(() => {
    setCheckState("idle");
    setCheckError(null);
  }, [httpUrl]);

  const handleValidate = () => {
    if (!httpUrl) return;
    setCheckState("checking");
    setCheckError(null);
    apiFetch(httpUrl)
      .then(() => setCheckState("ok"))
      .catch((e: unknown) => {
        setCheckState("error");
        setCheckError(friendlyError(e));
      });
  };

  const canDownload = checkState === "ok";

  return (
    <div className="card">
      <p className="card-label">Download</p>
      {mForm ? (
        <>
          <p className="card-description">
            Check your address and settings, then generate your link.
          </p>
          <button
            type="button"
            className="btn btn-primary"
            disabled={checkState === "checking" || checkState === "ok"}
            onClick={handleValidate}
          >
            {checkState === "checking" ? "Checking…" : "Validate"}
          </button>
          {checkState === "error" && checkError && (
            <p className="msg-error">{checkError}</p>
          )}
          {canDownload && (
            <>
              <p className="card-description" style={{ marginTop: "0.75rem" }}>
                Use the webcal link to subscribe — your calendar will stay up to
                date automatically.
              </p>
              <textarea
                className="url-display"
                readOnly
                value={mkWebcalLink(mForm)}
              />
              <div className="download-actions">
                <a className="btn btn-primary" href={mkWebcalLink(mForm)}>
                  Subscribe (webcal)
                </a>
                <a
                  className="btn"
                  download={filename}
                  href={mkHttpLink(mForm).toString()}
                >
                  Download .ics
                </a>
              </div>
            </>
          )}
        </>
      ) : (
        <p className="msg-empty">
          {formState.isValid
            ? "Fill in your address above to generate a link."
            : "Fix the highlighted fields above to generate a link."}
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
