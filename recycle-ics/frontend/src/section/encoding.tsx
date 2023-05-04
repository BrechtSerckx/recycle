import * as React from "react";
import { useFormContext, useWatch, useFieldArray } from "react-hook-form";

const EncodingRadio = React.forwardRef(
  ({ children, label, ...props }: any, ref: any) => (
    <div>
      <label>
        <input ref={ref} type="radio" {...props} />
        {label}:{children}
      </label>
    </div>
  )
);

const EventTimeInput = React.forwardRef(
  ({ label, ...props }: any, ref: any) => (
    <label>
      {label}
      <input ref={ref} type="time" {...props} />
    </label>
  )
);

const EventInputs = () => {
  const { control, register } = useFormContext();
  const value = "event";
  var isChecked = useWatch({ name: "fe" }) === value;
  const {
    fields: reminders,
    append,
    remove,
  } = useFieldArray({
    control,
    name: "reminders",
  });
  return (
    <EncodingRadio label="Event" value={value} {...register("fe")}>
      <p>Represent waste collections as an event.</p>
      <EventTimeInput
        label="Start time"
        disabled={!isChecked}
        {...register("es", { required: isChecked })}
      />
      <EventTimeInput
        label="End time"
        disabled={!isChecked}
        {...register("ee", { required: isChecked })}
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
      <button type="button" disabled={!isChecked} onClick={() => append({})}>
        Add reminder
      </button>
    </EncodingRadio>
  );
};

const TodoDaysBeforeInput = React.forwardRef(
  ({ label, ...props }: any, ref: any) => (
    <label>
      {label} <input ref={ref} type="number" {...props} />
    </label>
  )
);

const TodoTimeInput = React.forwardRef(({ label, ...props }: any, ref: any) => (
  <label>
    {label} <input ref={ref} type="time" {...props} />
  </label>
));

const TodoFullDayInputs = ({ isParentChecked }: any) => {
  const { register } = useFormContext();
  const value = "date";
  var isChecked = useWatch({ name: "tdt" }) === value;
  return (
    <EncodingRadio
      label="Full day"
      value={value}
      disabled={!isParentChecked}
      {...register("tdt")}
    >
      <p>
        <TodoDaysBeforeInput
          label="Days before"
          disabled={!(isParentChecked && isChecked)}
          {...register("tdb", {
            required: isParentChecked && isChecked,
          })}
        />
      </p>
    </EncodingRadio>
  );
};

const TodoSpecificTimeInputs = ({ isParentChecked }: any) => {
  const { register } = useFormContext();
  const value = "datetime";
  var isChecked = useWatch({ name: "tdt" }) === value;
  return (
    <EncodingRadio
      label="Specific time"
      value={value}
      disabled={!isParentChecked}
      {...register("tdt")}
    >
      <p>
        <TodoDaysBeforeInput
          label="Days before"
          disabled={!(isParentChecked && isChecked)}
          {...register("tdb", {
            required: isParentChecked && isChecked,
          })}
        />
        <TodoTimeInput
          label="Time"
          disabled={!(isParentChecked && isChecked)}
          {...register("tt", {
            required: isParentChecked && isChecked,
          })}
        />
      </p>
    </EncodingRadio>
  );
};

const TodoInputs = () => {
  const { register } = useFormContext();
  const value = "todo";
  var isChecked = useWatch({ name: "fe" }) === value;
  return (
    <EncodingRadio value={value} label="Todo" {...register("fe")}>
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
