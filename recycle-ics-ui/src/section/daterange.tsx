import * as React from "react";
import {
  useFormContext,
  UseFormRegisterReturn,
  useWatch,
} from "react-hook-form";
import { FormInputs } from "../types";

const DateRangeRadio = React.forwardRef(
  (
    {
      children,
      label,
      ...props
    }: Partial<UseFormRegisterReturn> & {
      label: React.ReactNode;
      children: React.ReactNode;
      value: string;
    },
    ref: React.ForwardedRef<HTMLInputElement>
  ) => (
    <div>
      <label>
        <input ref={ref} type="radio" {...props} />
        {label}:{children}
      </label>
    </div>
  )
);

const AbsoluteDateInput = React.forwardRef(
  (
    {
      label,
      ...props
    }: Partial<UseFormRegisterReturn> & {
      label: React.ReactNode;
    },
    ref: React.ForwardedRef<HTMLInputElement>
  ) => (
    <label>
      {label} <input ref={ref} type="date" {...props} />
    </label>
  )
);

const AbsoluteDateRangeInputs = () => {
  const { register } = useFormContext<FormInputs>();
  const value = "absolute";
  var isChecked = useWatch({ name: "dateRangeType" }) === value;
  return (
    <DateRangeRadio
      label="Absolute"
      value={value}
      {...register("dateRangeType")}
    >
      <p>This will get the waste collections between two dates.</p>
      <AbsoluteDateInput
        label="From: "
        disabled={!isChecked}
        {...register("absoluteDateRangeFrom", { required: isChecked })}
      />
      <AbsoluteDateInput
        label="To: "
        disabled={!isChecked}
        {...register("absoluteDateRangeTo", { required: isChecked })}
      />
    </DateRangeRadio>
  );
};

const RelativeDateInput = React.forwardRef(
  (
    {
      label,
      ...props
    }: Partial<UseFormRegisterReturn> & {
      label: React.ReactNode;
    },
    ref: React.ForwardedRef<HTMLInputElement>
  ) => (
    <label>
      {label}
      <input ref={ref} type="number" step={1} {...props} />
    </label>
  )
);

const RelativeDateRangeInputs = () => {
  const { register } = useFormContext<FormInputs>();
  const value = "relative";
  var isChecked = useWatch({ name: "dateRangeType" }) === value;
  return (
    <DateRangeRadio
      label="Relative"
      value="relative"
      {...register("dateRangeType")}
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
        disabled={!isChecked}
        {...register("relativeDateRangeFrom", {
          required: isChecked,
          value: -14,
        })}
      />
      <RelativeDateInput
        label="Days before:"
        disabled={!isChecked}
        {...register("relativeDateRangeTo", { required: isChecked, value: 28 })}
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
