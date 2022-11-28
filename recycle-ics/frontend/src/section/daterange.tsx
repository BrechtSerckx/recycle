import * as React from "react";

const DateRangeRadio = React.forwardRef(
  ({ children, label, description, ...props }: any, ref: any) => (
    <div>
      <label>
        <input ref={ref} type="radio" {...props} />
        {label}:{children}
      </label>
    </div>
  )
);

const AbsoluteDateInput = React.forwardRef(
  ({ label, ...props }: any, ref: any) => (
    <label>
      {label} <input type="date" {...props} />
    </label>
  )
);

const RelativeDateInput = React.forwardRef(
  ({ label, ...props }: any, ref: any) => (
    <label>
      {label}
      <input type="number" step={1} {...props} />
    </label>
  )
);

export default function DateRangeSection() {
  return (
    <>
      <h3>Date range</h3>
      <p>
        Choose a start date and an end date for which you want the waste
        collections.
      </p>
      <fieldset>
        <legend>Date range type</legend>
        <DateRangeRadio
          label="Absolute"
          name="drt"
          value="absolute"
          defaultChecked
        >
          <p>This will get the waste collections between two dates.</p>
          <AbsoluteDateInput label="From: " name="f" required />
          <AbsoluteDateInput label="To: " name="t" required />
        </DateRangeRadio>
        <DateRangeRadio label="Relative" name="drt" value="relative">
          <p>
            This will get the waste collections relative to the current date.
            This is is particularly useful when auto-importing the waste
            collections through Google Calendar or Outlook, as it will always
            give the collections relative to that date. The collections will
            always be up-to-date like this.
          </p>
          <RelativeDateInput label="Days before:" name="f" value="-14" />
          <RelativeDateInput label="Days before:" name="t" value="28" />
        </DateRangeRadio>
      </fieldset>
    </>
  );
}
