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
          <input
            type="text"
            readOnly
            placeholder="permalink"
            size={50}
            value={(mForm && mkWebcalLink(mForm).toString()) || undefined}
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
