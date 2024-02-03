import * as React from "react";

const howTos: {
  title: string;
  summary: React.ReactNode;
  howTo: React.ReactNode;
}[] = [
  {
    title: "ICSDroid / ICSx5",
    summary: (
      <>
        See <a href="https://icsx5.bitfire.at/usage/">ICSx5 Usage</a>.
      </>
    ),
    howTo: (
      <>
        <p>You can subscribe to iCalendars (.ics files) with two methods:</p>
        <ol>
          <li>
            Follow the webcal link in your browser or ics file in your local
            file manager. Select “ICSx⁵” if you’re asked which app shall open
            the link.
          </li>
          <li>Tap “+” in the ICSx⁵ main activity.</li>
        </ol>
        <p>
          In both cases, the ICSx⁵ “Add subscription” activity will appear.
          Click on “Next”.
        </p>
        <p>
          Then, enter a title and select a color for the calendar. You can
          change title and color later.
        </p>
      </>
    ),
  },
  {
    title: "Google Calendar",
    summary: (
      <>
        See{" "}
        <a href="https://support.google.com/calendar/answer/37100?hl=en&co=GENIE.Platform%3DDesktop">
          Subscribe to someone’s Google Calendar
        </a>
        .
      </>
    ),
    howTo: (
      <>
        <ol>
          <li>
            On your computer, open{" "}
            <a
              href="https://calendar.google.com/"
              rel="noreferrer"
              target="_blank"
            >
              Google Calendar
            </a>
            .
          </li>
          <li>
            On the left, next to "Other calendars," click Add{" "}
            <img
              src="//lh3.googleusercontent.com/acEIXV8pLgZ5PXG23uIE9Ioz8RISJeLeAUH8WlHFipigJvssMhq20nQgEWQcqWn1iyM=w36-h36"
              alt="Add other calendars"
              title="Add other calendars"
              data-mime-type="image/png"
              data-alt-src="//lh3.googleusercontent.com/acEIXV8pLgZ5PXG23uIE9Ioz8RISJeLeAUH8WlHFipigJvssMhq20nQgEWQcqWn1iyM"
              width="18"
              height="18"
            />
            &nbsp;
            <img
              src="//lh3.googleusercontent.com/3_l97rr0GvhSP2XV5OoCkV2ZDTIisAOczrSdzNCBxhIKWrjXjHucxNwocghoUa39gw=w36-h36"
              alt="and then"
              title="and then"
              data-mime-type="image/png"
              data-alt-src="//lh3.googleusercontent.com/3_l97rr0GvhSP2XV5OoCkV2ZDTIisAOczrSdzNCBxhIKWrjXjHucxNwocghoUa39gw"
              width="18"
              height="18"
            />{" "}
            <strong>From URL.</strong>
          </li>
          <li>Enter the link generated through this form.</li>
          <li>
            Click <strong>Add calendar. </strong>The calendar appears on the
            left, under "Other calendars."
          </li>
        </ol>
        <p>
          <strong>Tip:</strong> It might take up to 12 hours for changes to show
          in your Google Calendar.
        </p>
      </>
    ),
  },
  {
    title: "Outlook.com",
    summary: (
      <>
        See{" "}
        <a href="https://support.microsoft.com/en-us/topic/cff1429c-5af6-41ec-a5b4-74f2c278e98c">
          Import or subscribe to a calendar in Outlook.com
        </a>
      </>
    ),
    howTo: (
      <>
        <p>
          <strong>Note:</strong> When you subscribe to a calendar, your calendar
          will automatically refresh if the other calendar is updated. This can
          sometimes take more than 24 hours.
        </p>
        <ol type="1">
          <li>
            <a
              href="https://go.microsoft.com/fwlink/p/?linkid=843379"
              target="_blank"
              rel="noreferrer"
            >
              Sign in to Outlook.com
            </a>
            .
          </li>
          <li>
            At the bottom of the page, select{" "}
            <img
              src="https://support.content.office.net/en-us/media/b800323e-05b6-4501-a02c-2bafa723b06b.png"
              alt="Calendar"
              loading="lazy"
            />
            .{" "}
          </li>
          <li>
            In the navigation pane, select <b>Add calendar</b>.
          </li>
          <li>
            <p>
              Select <b>Subscribe from web</b>.
            </p>
            <p>
              <img
                src="https://support.content.office.net/en-us/media/ee5d0e9e-dc77-4873-a5b6-841e1c3b4c41.png"
                alt="Subscribe to a calendar"
                loading="lazy"
              />{" "}
            </p>
          </li>
          <li>Enter the URL for the calendar. </li>
          <li>
            Select <b>Import</b>.{" "}
          </li>
        </ol>
      </>
    ),
  },
];

export default function DescriptionSection() {
  return (
    <>
      <section>
        <h2>Description</h2>
        <p>Generate ICS files and links for your waste collections. </p>
        <p> Based on recycleapp.be and the Recycle! app.</p>
        <p>
          You can manually import the generated ICS files into your calendar,
          but let a tool like ICSx5/ICSDroid or a calendar app like Google
          Calendar or Outlook.com automatically import them for you. That way,
          your calendar always stays up to date with the waste collections.
          Note: ICSx5 works most reliably, Google Calendar for example does not
          support all features.
        </p>
        <h2>How-to</h2>
        {howTos.map(({ title, summary, howTo }, i) => (
          <div key={i}>
            <h3>{title}</h3>
            <details>
              <>
                <summary>{summary}</summary>
                {howTo}
              </>
            </details>
          </div>
        ))}
      </section>
    </>
  );
}
