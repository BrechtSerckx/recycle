import * as React from "react";
import { useFormContext, useWatch } from "react-hook-form";

const DateRangeRadio = React.forwardRef(
  ({ children, label, ...props }: any, ref: any) => (
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

const AbsoluteDateRangeInputs = () => {
  const { register } = useFormContext();
  const value = "absolute";
  const defaultChecked = true;
  var isChecked =
    useWatch({ name: "drt", defaultValue: defaultChecked && value }) === value;
  return (
    <DateRangeRadio
      label="Absolute"
      value={value}
      defaultChecked={defaultChecked}
      {...register("drt")}
    >
      <p>This will get the waste collections between two dates.</p>
      <AbsoluteDateInput
        label="From: "
        name="f"
        required
        disabled={!isChecked}
      />
      <AbsoluteDateInput label="To: " name="t" required disabled={!isChecked} />
    </DateRangeRadio>
  );
};

const RelativeDateInput = React.forwardRef(
  ({ label, ...props }: any, ref: any) => (
    <label>
      {label}
      <input type="number" step={1} {...props} />
    </label>
  )
);

const RelativeDateRangeInputs = () => {
  const { register } = useFormContext();
  const value = "relative";
  const defaultChecked = false;
  var isChecked =
    useWatch({ name: "drt", defaultValue: defaultChecked && value }) === value;
  return (
    <DateRangeRadio label="Relative" value="relative" {...register("drt")}>
      <p>
        This will get the waste collections relative to the current date. This
        is is particularly useful when auto-importing the waste collections
        through Google Calendar or Outlook, as it will always give the
        collections relative to that date. The collections will always be
        up-to-date like this.
      </p>
      <RelativeDateInput
        label="Days before:"
        name="f"
        defaultValue="-14"
        required
        disabled={!isChecked}
      />
      <RelativeDateInput
        label="Days before:"
        name="t"
        defaultValue="28"
        required
        disabled={!isChecked}
      />
    </DateRangeRadio>
  );
};

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
        <AbsoluteDateRangeInputs />
        <RelativeDateRangeInputs />
      </fieldset>
    </>
  );
}
