import * as React from "react";
import { useWatch } from "react-hook-form";
import { FormInputs, inputsToForm, Form, formToParams } from "../types";

export default function DownloadSection() {
  const formInputs = useWatch() as FormInputs,
    mForm = inputsToForm(formInputs);
  const mkUrl = (base: string, form: Form): URL => {
      var url = new URL("/api/generate", base);
      url.search = new URLSearchParams(formToParams(form)).toString();
      return url;
    },
    mkWebcalLink = (form: Form): URL =>
      mkUrl(`webcal://${window.location.host}`, form),
    mkHttpLink = (form: Form): URL => mkUrl(window.location.origin, form),
    filename = "recycle.ics";
  return (
    <>
      <section>
        <p>
          <textarea
            readOnly
            placeholder="Please fill in your address above to generate a permalink."
            wrap="soft"
            value={(mForm && mkWebcalLink(mForm).toString()) || undefined}
            style={{ width: "100%" }}
          />
        </p>
        {mForm && (
          <p>
            <a download={filename} href={mkWebcalLink(mForm).toString()}>
              Open
            </a>
            <a download={filename} href={mkHttpLink(mForm).toString()}>
              Download
            </a>
          </p>
        )}
      </section>
    </>
  );
}
