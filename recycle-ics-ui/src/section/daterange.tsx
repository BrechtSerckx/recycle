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
    <div className="radio-option">
      <label>
        <input ref={ref} type="radio" {...props} />
        {label}
      </label>
      <div className="option-body">{children}</div>
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
      {label}
      <input ref={ref} type="date" {...props} />
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
      <p className="card-description" style={{ margin: "0 0 0.75rem" }}>
        Collections between two fixed dates.
      </p>
      <AbsoluteDateInput
        label="From"
        disabled={!isChecked}
        {...register("absoluteDateRangeFrom", { required: isChecked })}
      />
      <AbsoluteDateInput
        label="To"
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
      <p className="card-description" style={{ margin: "0 0 0.75rem" }}>
        Collections relative to today — stays up to date when auto-imported.
      </p>
      <RelativeDateInput
        label="Days before today"
        disabled={!isChecked}
        {...register("relativeDateRangeFrom", {
          required: isChecked,
          value: -14,
        })}
      />
      <RelativeDateInput
        label="Days after today"
        disabled={!isChecked}
        {...register("relativeDateRangeTo", { required: isChecked, value: 28 })}
      />
    </DateRangeRadio>
  );
};

export default function DateRangeSection() {
  return (
    <div className="card">
      <p className="card-label">Date range</p>
      <p className="card-description">
        The period for which to fetch waste collections.
      </p>
      <div className="radio-options">
        <AbsoluteDateRangeInputs />
        <RelativeDateRangeInputs />
      </div>
    </div>
  );
}
