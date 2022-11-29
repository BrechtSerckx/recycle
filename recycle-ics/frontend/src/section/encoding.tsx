import * as React from "react";

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
        <label>
          <input type="radio" name="fe" value="event" />
          Event
        </label>
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
        <label>
          <input type="radio" name="fe" value="todo" defaultChecked />
          Todo
        </label>
        <p>
          Represent waste collections as a todo or task.
          <strong>Does not work with Google Calendar! </strong>Maybe with
          Outlook, that's untested.
        </p>
        <fieldset>
          <legend>Due type</legend>
          <label>
            <input type="radio" name="tdt" value="date" />
            Full day:
          </label>
          <TodoDaysBeforeInput
            label="Days before"
            name="tdb"
            required
            value="1"
            disabled
          />
          <label>
            <input type="radio" name="tdt" value="datetime" />
            Specific time:
          </label>
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
        </fieldset>
      </fieldset>
    </>
  );
}
