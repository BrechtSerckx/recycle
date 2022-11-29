import * as React from "react";

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
  return (
    <>
      <h3>Encoding</h3>
      <fieldset>
        <legend>Fraction encoding</legend>
        <EncodingRadio label="Event" name="fe" value="event">
          <p>Represent waste collections as an event.</p>
          <EventTimeInput
            label="Start time"
            name="es"
            required
            value="07:00"
            disabled
          />
          <EventTimeInput
            label="End time"
            name="ee"
            required
            value="10:00"
            disabled
          />
          <p>Reminders: </p>
          <ul id="reminder_list"> </ul>
          <button id="add_reminder_button" type="button">
            Add reminder
          </button>
        </EncodingRadio>
        <EncodingRadio label="Todo" name="fe" value="todo">
          <p>
            Represent waste collections as a todo or task.
            <strong>Does not work with Google Calendar! </strong>Maybe with
            Outlook, that's untested.
          </p>
          <fieldset>
            <legend>Due type</legend>
            <EncodingRadio label="Full day" name="tdt" value="date">
              <TodoDaysBeforeInput
                label="Days before"
                name="tdb"
                required
                value="1"
                disabled
              />
            </EncodingRadio>
            <EncodingRadio label="Specific time" name="tdt" value="datetime">
              <TodoDaysBeforeInput
                label="Days before"
                name="tdb"
                required
                value="1"
                disabled
              />
              <TodoTimeInput
                label="Time"
                name="tt"
                required
                value="20:00"
                disabled
              />
            </EncodingRadio>
          </fieldset>
        </EncodingRadio>
      </fieldset>
    </>
  );
}
