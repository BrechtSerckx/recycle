import * as React from "react";
import { useFormContext } from "react-hook-form";

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
      {label} <input type="time" {...props} />
    </label>
  )
);

const EventInputs = ({ isChecked }: any) => {
  const { register } = useFormContext();
  return (
    <>
      <p>Represent waste collections as an event.</p>
      <EventTimeInput
        label="Start time"
        value="07:00"
        disabled={!isChecked}
        {...register("es", { required: true })}
      />
      <EventTimeInput
        label="End time"
        value="10:00"
        disabled={!isChecked}
        {...register("ee", { required: true })}
      />
      <p>Reminders: </p>
      <ul id="reminder_list"> </ul>
      <button id="add_reminder_button" type="button">
        Add reminder
      </button>
    </>
  );
};

const TodoDaysBeforeInput = React.forwardRef(
  ({ label, ...props }: any, ref: any) => (
    <label>
      {label} <input type="number" {...props} />
    </label>
  )
);

const TodoTimeInput = React.forwardRef(({ label, ...props }: any, ref: any) => (
  <label>
    {label} <input type="time" {...props} />
  </label>
));

const TodoFullDayInputs = ({ isChecked }: any) => {
  const { register } = useFormContext();
  return (
    <TodoDaysBeforeInput
      label="Days before"
      value="1"
      disabled={!isChecked}
      {...register("tdb", { required: true })}
    />
  );
};

const TodoSpecificTimeInputs = ({ isChecked }: any) => {
  const { register } = useFormContext();
  return (
    <>
      <TodoDaysBeforeInput
        label="Days before"
        value="1"
        disabled={!isChecked}
        {...register("tdb", { required: true })}
      />
      <TodoTimeInput
        label="Time"
        value="20:00"
        disabled={!isChecked}
        {...register("tt", { required: true })}
      />
    </>
  );
};

const TodoInputs = ({ isParentChecked }: any) => {
  const { register, watch } = useFormContext();
  const dueTypes = [
    {
      label: "Full day",
      value: "date",
      mkChildren: (isChecked: boolean) => (
        <TodoFullDayInputs isChecked={isChecked} />
      ),
    },
    {
      label: "Specific time",
      value: "datetime",
      defaultChecked: true,
      mkChildren: (isChecked: boolean) => (
        <TodoSpecificTimeInputs isChecked={isChecked} />
      ),
    },
  ];
  return (
    <>
      <p>
        Represent waste collections as a todo or task.
        <strong>Does not work with Google Calendar! </strong>Maybe with Outlook,
        that's untested.
      </p>
      <fieldset>
        <legend>Due type</legend>
        {dueTypes.map(({ mkChildren, ...props }: any) => {
          const { value, defaultChecked } = props;
          return (
            <EncodingRadio
              key={value}
              {...props}
              disabled={!isParentChecked}
              {...register("tdt")}
            >
              {mkChildren(
                isParentChecked &&
                  watch("tdt", defaultChecked && value) === value
              )}
            </EncodingRadio>
          );
        })}
      </fieldset>
    </>
  );
};

export default function EncodingSection() {
  const { register, watch } = useFormContext();
  const encodings = [
    {
      label: "Event",
      value: "event",
      defaultChecked: true,
      mkChildren: (isChecked: boolean) => <EventInputs isChecked={isChecked} />,
    },
    {
      label: "Todo",
      value: "todo",
      mkChildren: (isChecked: boolean) => (
        <TodoInputs isParentChecked={isChecked} />
      ),
    },
  ];
  return (
    <>
      <h3>Encoding</h3>
      <fieldset>
        <legend>Fraction encoding</legend>
        {encodings.map(({ mkChildren, ...props }: any) => {
          const { value, defaultChecked } = props;
          return (
            <EncodingRadio key={value} {...props} {...register("fe")}>
              {mkChildren(watch("fe", defaultChecked && value) === value)}
            </EncodingRadio>
          );
        })}
      </fieldset>
    </>
  );
}
