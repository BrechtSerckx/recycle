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
    <div>
      <label>
        <input ref={ref} type="radio" {...props} />
        {label}:{children}
      </label>
    </div>
  )
);

const EventTimeInput = React.forwardRef(
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
      <input ref={ref} type="time" {...props} />
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
  } = useFieldArray({
    control,
    name: "reminders",
  });
  return (
    <EncodingRadio
      label="Event"
      value={value}
      {...register("fractionEncodingType")}
    >
      <p>Represent waste collections as an event.</p>
      <EventTimeInput
        label="Start time"
        disabled={!isChecked}
        {...register("feEventStart", { required: isChecked })}
      />
      <EventTimeInput
        label="End time"
        disabled={!isChecked}
        {...register("feEventEnd", { required: isChecked })}
      />
      <p>Reminders: </p>
      <ul id="reminder_list">
        {reminders.map((reminder, index) => (
          <li key={reminder.id}>
            <fieldset>
              <legend>Reminder</legend>
              <label>
                Days before:
                <input
                  type="number"
                  disabled={!isChecked}
                  {...register(`reminders.${index}.rdb`, {
                    required: true,
                    value: 0,
                  })}
                />
              </label>
              <label>
                Hours before:
                <input
                  type="number"
                  disabled={!isChecked}
                  {...register(`reminders.${index}.rhb`, {
                    required: true,
                    value: 10,
                  })}
                />
              </label>
              <label>
                Minutes before:
                <input
                  type="number"
                  disabled={!isChecked}
                  {...register(`reminders.${index}.rmb`, {
                    required: true,
                    value: 0,
                  })}
                />
              </label>
              <button
                type="button"
                disabled={!isChecked}
                onClick={() => remove(index)}
              >
                Delete reminder
              </button>
            </fieldset>
          </li>
        ))}
      </ul>
      <button
        type="button"
        disabled={!isChecked}
        onClick={() => append({} as any)}
      >
        Add reminder
      </button>
    </EncodingRadio>
  );
};

const TodoDaysBeforeInput = React.forwardRef(
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
      {label} <input ref={ref} type="number" {...props} />
    </label>
  )
);

const TodoTimeInput = React.forwardRef(
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
      {label} <input ref={ref} type="time" {...props} />
    </label>
  )
);

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
      <p>
        <TodoDaysBeforeInput
          label="Days before"
          disabled={!(isParentChecked && isChecked)}
          {...register("feTodoDueDateDaysBefore", {
            required: isParentChecked && isChecked,
          })}
        />
      </p>
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
      <p>
        <TodoDaysBeforeInput
          label="Days before"
          disabled={!(isParentChecked && isChecked)}
          {...register("feTodoDueDatetimeDaysBefore", {
            required: isParentChecked && isChecked,
          })}
        />
        <TodoTimeInput
          label="Time"
          disabled={!(isParentChecked && isChecked)}
          {...register("feTodoDueDatetimeTimeOfDay", {
            required: isParentChecked && isChecked,
          })}
        />
      </p>
    </EncodingRadio>
  );
};

const TodoInputs = () => {
  const { register } = useFormContext<FormInputs>();
  const value = "todo";
  var isChecked = useWatch({ name: "fractionEncodingType" }) === value;
  return (
    <EncodingRadio
      value={value}
      label="Todo"
      {...register("fractionEncodingType")}
    >
      <p>
        Represent waste collections as a todo or task.
        <strong>Does not work with Google Calendar! </strong>Maybe with Outlook,
        that's untested.
      </p>
      <fieldset>
        <legend>Due type</legend>
        <TodoFullDayInputs isParentChecked={isChecked} />
        <TodoSpecificTimeInputs isParentChecked={isChecked} />
      </fieldset>
    </EncodingRadio>
  );
};

export default function EncodingSection() {
  return (
    <>
      <h3>Encoding</h3>
      <fieldset>
        <legend>Fraction encoding</legend>
        <EventInputs />
        <TodoInputs />
      </fieldset>
    </>
  );
}
