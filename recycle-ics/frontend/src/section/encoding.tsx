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

export default function EncodingSection() {
  const { register, watch } = useFormContext();
  const encodings = [
    {
      label: "Event",
      value: "event",
      defaultChecked: true,
      mkChildren: (isChecked: boolean) => (
        <>
          <p>Represent waste collections as an event.</p>
          <EventTimeInput
            label="Start time"
            name="es"
            required
            value="07:00"
            disabled={!isChecked}
          />
          <EventTimeInput
            label="End time"
            name="ee"
            required
            value="10:00"
            disabled={!isChecked}
          />
          <p>Reminders: </p>
          <ul id="reminder_list"> </ul>
          <button id="add_reminder_button" type="button">
            Add reminder
          </button>
        </>
      ),
    },
    {
      label: "Todo",
      value: "todo",
      mkChildren: (isChecked: boolean) => {
        const dueTypes = [
          {
            label: "Full day",
            value: "date",
            mkChildren: (isChecked2: boolean) => (
              <TodoDaysBeforeInput
                label="Days before"
                name="tdb"
                required
                value="1"
                disabled={!(isChecked && isChecked2)}
              />
            ),
          },
          {
            label: "Specific time",
            value: "datetime",
            defaultChecked: true,
            mkChildren: (isChecked2: boolean) => (
              <>
                <TodoDaysBeforeInput
                  label="Days before"
                  name="tdb"
                  required
                  value="1"
                  disabled={!(isChecked && isChecked2)}
                />
                <TodoTimeInput
                  label="Time"
                  name="tt"
                  required
                  value="20:00"
                  disabled={!(isChecked && isChecked2)}
                />
              </>
            ),
          },
        ];
        return (
          <>
            <p>
              Represent waste collections as a todo or task.
              <strong>Does not work with Google Calendar! </strong>Maybe with
              Outlook, that's untested.
            </p>
            <fieldset>
              <legend>Due type</legend>
              {dueTypes.map(({ mkChildren, ...props }: any) => {
                const { value, defaultChecked } = props;
                return (
                  <EncodingRadio
                    key={value}
                    {...props}
                    disabled={!isChecked}
                    {...register("tdt")}
                  >
                    {mkChildren(
                      watch("tdt", isChecked && defaultChecked && value) ===
                        value
                    )}
                  </EncodingRadio>
                );
              })}
            </fieldset>
          </>
        );
      },
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
