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
        <label>
          Start time:
          <input type="time" name="es" required value="07:00" disabled />
        </label>
        <label>
          End time:
          <input type="time" name="ee" required value="10:00" disabled />
        </label>
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
          <label>
            Days before:
            <input type="number" name="tdb" required value="1" disabled />
          </label>
          <label>
            <input type="radio" name="tdt" value="datetime" />
            Specific time:
          </label>
          <label>
            Days before:
            <input type="number" name="tdb" required value="1" disabled />
          </label>
          <label>
            Time:
            <input type="time" name="tt" required value="20:00" disabled />
          </label>
        </fieldset>
      </fieldset>
    </>
  );
}
