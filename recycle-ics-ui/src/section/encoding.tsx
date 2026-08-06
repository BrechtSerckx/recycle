import * as React from "react";
import {
  useFormContext,
  useWatch,
  useFieldArray,
  UseFormRegisterReturn,
} from "react-hook-form";
import { FormInputs } from "../types";

const EncodingRadio = React.forwardRef(
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

const TimeInput = React.forwardRef(
  (
    {
      label,
      ...props
    }: Partial<UseFormRegisterReturn> & { label: React.ReactNode },
    ref: React.ForwardedRef<HTMLInputElement>
  ) => (
    <label>
      {label}
      <input ref={ref} type="time" {...props} />
    </label>
  )
);

const NumberInput = React.forwardRef(
  (
    {
      label,
      ...props
    }: Partial<UseFormRegisterReturn> & { label: React.ReactNode },
    ref: React.ForwardedRef<HTMLInputElement>
  ) => (
    <label>
      {label}
      <input ref={ref} type="number" {...props} />
    </label>
  )
);

const EventInputs = () => {
  const { control, register } = useFormContext<FormInputs>();
  const value = "event";
  var isChecked = useWatch({ name: "fractionEncodingType" }) === value;
  const {
    fields: reminders,
    append,
    remove,
  } = useFieldArray({ control, name: "reminders" });
  return (
    <EncodingRadio label="Event" value={value} {...register("fractionEncodingType")}>
      <p className="card-description" style={{ margin: "0 0 0.75rem" }}>
        Represent waste collections as calendar events.
      </p>
      <TimeInput
        label="Start time"
        disabled={!isChecked}
        {...register("feEventStart", { required: isChecked })}
      />
      <TimeInput
        label="End time"
        disabled={!isChecked}
        {...register("feEventEnd", { required: isChecked })}
      />
      {reminders.length > 0 && (
        <ul className="reminder-list">
          {reminders.map((reminder, index) => (
            <li key={reminder.id} className="reminder-item">
              <NumberInput
                label="Days before"
                disabled={!isChecked}
                {...register(`reminders.${index}.rdb`, {
                  required: true,
                  value: 0,
                })}
              />
              <NumberInput
                label="Hours before"
                disabled={!isChecked}
                {...register(`reminders.${index}.rhb`, {
                  required: true,
                  value: 10,
                })}
              />
              <NumberInput
                label="Minutes before"
                disabled={!isChecked}
                {...register(`reminders.${index}.rmb`, {
                  required: true,
                  value: 0,
                })}
              />
              <button
                type="button"
                className="btn-danger"
                disabled={!isChecked}
                onClick={() => remove(index)}
              >
                Remove
              </button>
            </li>
          ))}
        </ul>
      )}
      <button
        type="button"
        disabled={!isChecked}
        onClick={() => append({} as any)}
        style={{ marginTop: "0.375rem" }}
      >
        + Add reminder
      </button>
    </EncodingRadio>
  );
};

const TodoFullDayInputs = ({
  isParentChecked,
}: {
  isParentChecked: boolean;
}) => {
  const { register } = useFormContext<FormInputs>();
  const value = "date";
  var isChecked = useWatch({ name: "feTodoDueType" }) === value;
  return (
    <EncodingRadio
      label="Full day"
      value={value}
      disabled={!isParentChecked}
      {...register("feTodoDueType")}
    >
      <NumberInput
        label="Days before"
        disabled={!(isParentChecked && isChecked)}
        {...register("feTodoDueDateDaysBefore", {
          required: isParentChecked && isChecked,
        })}
      />
    </EncodingRadio>
  );
};

const TodoSpecificTimeInputs = ({
  isParentChecked,
}: {
  isParentChecked: boolean;
}) => {
  const { register } = useFormContext<FormInputs>();
  const value = "datetime";
  var isChecked = useWatch({ name: "feTodoDueType" }) === value;
  return (
    <EncodingRadio
      label="Specific time"
      value={value}
      disabled={!isParentChecked}
      {...register("feTodoDueType")}
    >
      <NumberInput
        label="Days before"
        disabled={!(isParentChecked && isChecked)}
        {...register("feTodoDueDatetimeDaysBefore", {
          required: isParentChecked && isChecked,
        })}
      />
      <TimeInput
        label="Time of day"
        disabled={!(isParentChecked && isChecked)}
        {...register("feTodoDueDatetimeTimeOfDay", {
          required: isParentChecked && isChecked,
        })}
      />
    </EncodingRadio>
  );
};

const TodoInputs = () => {
  const { register } = useFormContext<FormInputs>();
  const value = "todo";
  var isChecked = useWatch({ name: "fractionEncodingType" }) === value;
  return (
    <EncodingRadio value={value} label="Todo" {...register("fractionEncodingType")}>
      <p className="card-description" style={{ margin: "0 0 0.75rem" }}>
        Represent waste collections as tasks/todos.{" "}
        <strong>Not supported by Google Calendar.</strong>
      </p>
      <p className="card-section-title" style={{ margin: "0 0 0.5rem" }}>
        Due type
      </p>
      <div className="radio-options">
        <TodoFullDayInputs isParentChecked={isChecked} />
        <TodoSpecificTimeInputs isParentChecked={isChecked} />
      </div>
    </EncodingRadio>
  );
};

export default function EncodingSection() {
  return (
    <div className="card">
      <p className="card-label">Encoding</p>
      <p className="card-description">
        How to represent waste collections in the calendar.
      </p>
      <div className="radio-options">
        <EventInputs />
        <TodoInputs />
      </div>
    </div>
  );
}
