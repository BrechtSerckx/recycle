import * as React from "react";
import { useWatch } from "react-hook-form";
import { FormInputs, inputsToForm, Form, formToParams } from "../types";
import { serverUrl, nodeEnv } from "../env";

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
          <div className="download-actions">
            <a
              className="btn btn-primary"
              href={mkWebcalLink(mForm)}
            >
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
