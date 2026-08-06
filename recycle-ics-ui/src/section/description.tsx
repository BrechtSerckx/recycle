import * as React from "react";
import { githubUrl } from "../env";

const howTos: {
  title: string;
  summary: React.ReactNode;
  howTo: React.ReactNode;
}[] = [
  {
    title: "ICSx⁵ / ICSDroid",
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
            Follow the webcal link in your browser. Select "ICSx⁵" when asked
            which app to open the link with.
          </li>
          <li>Tap "+" in the ICSx⁵ main activity.</li>
        </ol>
        <p>
          The ICSx⁵ "Add subscription" activity will appear. Click "Next", then
          enter a title and color for the calendar.
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
          Subscribe to someone's Google Calendar
        </a>
        .
      </>
    ),
    howTo: (
      <>
        <ol>
          <li>
            Open{" "}
            <a
              href="https://calendar.google.com/"
              rel="noreferrer"
              target="_blank"
            >
              Google Calendar
            </a>{" "}
            on your computer.
          </li>
          <li>
            On the left, next to "Other calendars," click Add &gt; From URL.
          </li>
          <li>Enter the link generated through this form.</li>
          <li>
            Click <strong>Add calendar</strong>.
          </li>
        </ol>
        <p>
          <strong>Note:</strong> It can take up to 12 hours for changes to
          appear in Google Calendar.
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
        .
      </>
    ),
    howTo: (
      <>
        <ol>
          <li>
            <a
              href="https://go.microsoft.com/fwlink/p/?linkid=843379"
              target="_blank"
              rel="noreferrer"
            >
              Sign in to Outlook.com
            </a>{" "}
            and go to Calendar.
          </li>
          <li>
            In the navigation pane, select <b>Add calendar</b> &gt;{" "}
            <b>Subscribe from web</b>.
          </li>
          <li>Enter the URL for the calendar.</li>
          <li>
            Select <b>Import</b>.
          </li>
        </ol>
        <p>
          <strong>Note:</strong> Subscribed calendars may take more than 24
          hours to refresh.
        </p>
      </>
    ),
  },
];

const changelog: { date: string; entries: string[] }[] = [
  {
    date: "2026-08-06",
    entries: [
      "Switched to public Fostplus API (upstream API changed)",
      "Better error reporting",
      "UI overhaul",
    ],
  },
  {
    date: "Start of changelog",
    entries: [],
  },
];

export function ChangelogSection() {
  return (
    <div className="card" style={{ marginTop: "1.5rem" }}>
      <p className="card-label">Changelog</p>
      <div className="howto-list">
        {changelog.map(({ date, entries }, i) => (
          <details key={i}>
            <summary>{date}</summary>
            {entries.length > 0 && (
              <div>
                <ul>
                  {entries.map((e, j) => (
                    <li key={j}>{e}</li>
                  ))}
                </ul>
              </div>
            )}
          </details>
        ))}
      </div>
    </div>
  );
}

export default function DescriptionSection() {
  return (
    <div className="card" style={{ marginBottom: "1.5rem" }}>
      <p className="card-description" style={{ margin: "0 0 0.5rem" }}>
        Generate ICS calendar files for waste collections from{" "}
        <a href="https://recycleapp.be" target="_blank" rel="noreferrer">
          recycleapp.be
        </a>
        . Open source on{" "}
        <a href={githubUrl} target="_blank" rel="noreferrer">
          GitHub
        </a>
        .
      </p>
      <p className="card-description" style={{ margin: "0 0 1rem" }}>
        <strong>Privacy:</strong> No data is stored. The service is completely
        stateless — your address is encoded directly in the webcal link and
        converted to collection data on the fly. Nothing is kept on the server.
      </p>
      <p className="card-section-title">How to subscribe</p>
      <div className="howto-list">
        {howTos.map(({ title, summary, howTo }, i) => (
          <details key={i}>
            <summary>{title} — {summary}</summary>
            <div>{howTo}</div>
          </details>
        ))}
      </div>
    </div>
  );
}
