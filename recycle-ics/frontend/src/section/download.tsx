import * as React from "react";
import { useWatch } from "react-hook-form";
import { FormInputs, inputsToForm } from "../types";

export default function DownloadSection() {
  const formValues = useWatch() as FormInputs;
  const path = "/foobar",
    host = window.location.host,
    webcalLink = `webcal://${host}${path}`,
    httpLink = path,
    filename = "recycle.ics";
  return (
    <>
      <section>
        <p>{JSON.stringify(formValues)}</p>
        <p>{JSON.stringify(inputsToForm(formValues))}</p>
        <p>
          <input
            type="text"
            readOnly
            placeholder="permalink"
            size={50}
            value={webcalLink}
          />
        </p>
        <p>
          <a download={filename} href={webcalLink}>
            Open
          </a>
          <a download={filename} href={httpLink}>
            Download
          </a>
        </p>
      </section>
    </>
  );
}
