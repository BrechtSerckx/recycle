const AbsoluteDateRangeInputs = () => (
  <div>
    <label>
      <input type="radio" name="drt" value="absolute" defaultChecked />
      Absolute:
      <p>This will get the waste collections between two dates.</p>
      <label>
        From:
        <input type="date" name="f" required />
      </label>
      <label>
        To:
        <input type="date" name="t" required />
      </label>
    </label>
  </div>
);
const RelativeDateRangeInputs = () => (
  <div>
    <label>
      <input type="radio" name="drt" value="relative" />
      Relative:
      <p>
        This will get the waste collections relative to the current date. This
        is is particularly useful when auto-importing the waste collections
        through Google Calendar or Outlook, as it will always give the
        collections relative to that date. The collections will always be
        up-to-date like this.
      </p>
      <label>
        Days before:
        <input type="number" step={1} name="f" required value="-14" disabled />
      </label>
      <label>
        Days after:
        <input type="number" step={1} name="t" required value="28" disabled />
      </label>
    </label>
  </div>
);
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
