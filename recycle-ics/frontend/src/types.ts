import { LangCode } from "./section/language";

export type FormInputs = {
  lc: LangCode;
  zipcode_q: string;
  zipcode_id: string;
  zipcode_name: string;
  street_q: string;
  street_id: string;
  street_name: string;
  house_number: string;
  drt: "absolute" | "relative";
  adrf: string;
  adrt: string;
  rdrf: number;
  rdrt: number;
  fe: "event" | "todo";
  es: string;
  ee: string;
  reminders: { rdb: number; rhb: number; rmb: number }[];
  tdt: "date" | "datetime";
  tdb: number;
  tt: string;
};

export type Form = {
  lc: LangCode;
  zipcode_id: string;
  street_id: string;
  house_number: string;
  date_range:
    | { drt: "absolute"; f: string; t: string }
    | { drt: "relative"; f: number; t: number };
  fraction_encoding:
    | {
        fe: "event";
        es: string;
        ee: string;
        reminders: { rdb: number; rhb: number; rmb: number }[];
      }
    | {
        fe: "todo";
        todo_due:
          | { tdt: "date"; tdb: number }
          | { tdt: "datetime"; tdb: number; tt: string };
      };
};
export const inputsToForm = ({
  lc,
  zipcode_id,
  street_id,
  house_number,
  drt,
  adrf,
  adrt,
  rdrf,
  rdrt,
  fe,
  es,
  ee,
  reminders,
  tdt,
  tdb,
  tt,
}: FormInputs): Form | null => {
  if (!lc || !zipcode_id || !street_id || !house_number) {
    return null;
  }
  var date_range;
  switch (drt) {
    case "absolute":
      date_range = { drt: drt as "absolute", f: adrf, t: adrt };
      break;
    case "relative":
      date_range = { drt: drt as "relative", f: rdrf, t: rdrt };
      break;
    default:
      return null;
  }
  var fraction_encoding;
  switch (fe) {
    case "event":
      fraction_encoding = { fe: fe as "event", es, ee, reminders };
      break;
    case "todo":
      var todo_due;
      switch (tdt) {
        case "date":
          todo_due = { tdt: tdt as "date", tdb };
          break;
        case "datetime":
          todo_due = { tdt: tdt as "datetime", tdb, tt };
          break;
        default:
          return null;
      }
      fraction_encoding = { fe: fe as "todo", todo_due };
      break;
    default:
      return null;
  }
  return {
    lc,
    zipcode_id,
    street_id,
    house_number,
    date_range,
    fraction_encoding,
  };
};

export const defaultFormInputs: Partial<FormInputs> = {
  lc: LangCode.NL,
  drt: "relative",
  fe: "event",
  es: "07:00",
  ee: "10:00",
  tdb: 1,
  tdt: "datetime",
  reminders: [],
  tt: "20:00",
};

export const formToParams = ({
  lc,
  zipcode_id,
  street_id,
  house_number,
  date_range,
  fraction_encoding,
}: Form): URLSearchParams => {
  var params = new URLSearchParams({
    lc,
    z: zipcode_id,
    s: street_id,
    hn: house_number,
  });

  params.append("drt", date_range.drt);
  switch (date_range.drt) {
    case "absolute":
      params.append("f", date_range.f);
      params.append("t", date_range.t);
      break;
    case "relative":
      params.append("f", date_range.f.toString());
      params.append("t", date_range.t.toString());
      break;
  }

  params.append("fe", fraction_encoding.fe);
  switch (fraction_encoding.fe) {
    case "event":
      params.append("es", fraction_encoding.es);
      params.append("ee", fraction_encoding.ee);
      for (const { rdb, rhb, rmb } of fraction_encoding.reminders) {
        params.append("rdb", rdb.toString());
        params.append("rhb", rhb.toString());
        params.append("rmb", rmb.toString());
      }
      break;
    case "todo":
      params.append("tdt", fraction_encoding.todo_due.tdt);
      switch (fraction_encoding.todo_due.tdt) {
        case "date":
          params.append("tdb", fraction_encoding.todo_due.tdb.toString());
          break;
        case "datetime":
          params.append("tdb", fraction_encoding.todo_due.tdb.toString());
          params.append("tt", fraction_encoding.todo_due.tt);
          break;
      }
      break;
  }
  return params;
};
