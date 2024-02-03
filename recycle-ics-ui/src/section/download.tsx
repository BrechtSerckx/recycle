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
    mkWebcalLink = (form: Form): URL => {
      var url = mkHttpLink(form);
      url.protocol = "webcal:";
      return url;
    },
    filename = "recycle.ics";
  return (
    <>
      <section>
        <p>
          <textarea
            readOnly
            placeholder="Please fill in your address above to generate a permalink."
            wrap="soft"
            value={(mForm && mkWebcalLink(mForm).toString()) || ""}
            style={{ width: "100%" }}
          />
        </p>
        {mForm && (
          <p>
            <a download={filename} href={mkWebcalLink(mForm).toString()}>
              Open
            </a>
            <span> - </span>
            <a download={filename} href={mkHttpLink(mForm).toString()}>
              Download
            </a>
          </p>
        )}
        {nodeEnv === "development" && (
          <>
            <h3>Raw form:</h3>
            <pre>{JSON.stringify(formInputs, null, 2)}</pre>
            <h3>Structured form:</h3>
            <pre>{JSON.stringify(mForm, null, 2)}</pre>
          </>
        )}
      </section>
    </>
  );
}
