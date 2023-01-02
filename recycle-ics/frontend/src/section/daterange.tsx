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
      {label} <input ref={ref} type="date" {...props} />
    </label>
  )
);

const AbsoluteDateRangeInputs = () => {
  const { register } = useFormContext();
  const value = "absolute";
  const defaultChecked = false;
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
        required
        disabled={!isChecked}
        {...register("adrf")} /* FIXME: convert to legacy `f` and `t` */
      />
      <AbsoluteDateInput
        label="To: "
        required
        disabled={!isChecked}
        {...register("adrt")}
      />
    </DateRangeRadio>
  );
};

const RelativeDateInput = React.forwardRef(
  ({ label, ...props }: any, ref: any) => (
    <label>
      {label}
      <input ref={ref} type="number" step={1} {...props} />
    </label>
  )
);

const RelativeDateRangeInputs = () => {
  const { register } = useFormContext();
  const value = "relative";
  const defaultChecked = true;
  var isChecked =
    useWatch({ name: "drt", defaultValue: defaultChecked && value }) === value;
  return (
    <DateRangeRadio
      label="Relative"
      value="relative"
      defaultChecked={defaultChecked}
      {...register("drt")}
    >
      <p>
        This will get the waste collections relative to the current date. This
        is is particularly useful when auto-importing the waste collections
        through Google Calendar or Outlook, as it will always give the
        collections relative to that date. The collections will always be
        up-to-date like this.
      </p>
      <RelativeDateInput
        label="Days before:"
        defaultValue={-14}
        disabled={!isChecked}
        {...register("rdrf")}
      />
      <RelativeDateInput
        label="Days before:"
        defaultValue={28}
        disabled={!isChecked}
        {...register("rdrt")}
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
