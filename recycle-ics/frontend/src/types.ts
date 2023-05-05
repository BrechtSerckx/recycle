import { LangCode } from "./section/language";

export type FormInputs = {
  langCode: LangCode;
  zipcodeQuery: string;
  zipcodeId: string;
  zipcodeName: string;
  streetQuery: string;
  streetId: string;
  streetName: string;
  houseNumber: string;
  filterAllEvents: boolean;
  filterAllFractions: boolean;
  filterSelectedFractions: string[];
  dateRangeType: "absolute" | "relative";
  absoluteDateRangeFrom: string;
  absoluteDateRangeTo: string;
  relativeDateRangeFrom: number;
  relativeDateRangeTo: number;
  fractionEncodingType: "event" | "todo";
  feEventStart: string;
  feEventEnd: string;
  reminders: { rdb: number; rhb: number; rmb: number }[];
  feTodoDueType: "date" | "datetime";
  feTodoDueDaysBefore: number;
  feTodoDueTimeOfDay: string;
};

export type Form = {
  langCode: LangCode;
  zipcodeId: string;
  streetId: string;
  houseNumber: string;
  filterAllEvents: boolean;
  filterAllFractions: boolean;
  filterSelectedFractions: string[];
  dateRange:
    | { type: "absolute"; from: string; to: string }
    | { type: "relative"; from: number; to: number };
  fractionEncoding:
    | {
        type: "event";
        start: string;
        end: string;
        reminders: { rdb: number; rhb: number; rmb: number }[];
      }
    | {
        type: "todo";
        due:
          | { type: "date"; date: number }
          | { type: "datetime"; date: number; time: string };
      };
};
export const inputsToForm = ({
  langCode,
  zipcodeId,
  streetId,
  houseNumber,
  filterAllEvents,
  filterAllFractions,
  filterSelectedFractions,
  dateRangeType,
  absoluteDateRangeFrom,
  absoluteDateRangeTo,
  relativeDateRangeFrom,
  relativeDateRangeTo,
  fractionEncodingType,
  feEventStart,
  feEventEnd,
  reminders,
  feTodoDueType,
  feTodoDueDaysBefore,
  feTodoDueTimeOfDay,
}: FormInputs): Form | null => {
  if (!langCode || !zipcodeId || !streetId || !houseNumber) {
    return null;
  }
  var dateRange;
  switch (dateRangeType) {
    case "absolute":
      dateRange = {
        type: dateRangeType as "absolute",
        from: absoluteDateRangeFrom,
        to: absoluteDateRangeTo,
      };
      break;
    case "relative":
      dateRange = {
        type: dateRangeType as "relative",
        from: relativeDateRangeFrom,
        to: relativeDateRangeTo,
      };
      break;
    default:
      return null;
  }
  var fractionEncoding;
  switch (fractionEncodingType) {
    case "event":
      fractionEncoding = {
        type: fractionEncodingType as "event",
        start: feEventStart,
        end: feEventEnd,
        reminders,
      };
      break;
    case "todo":
      var todoDue;
      switch (feTodoDueType) {
        case "date":
          todoDue = {
            type: feTodoDueType as "date",
            date: feTodoDueDaysBefore,
          };
          break;
        case "datetime":
          todoDue = {
            type: feTodoDueType as "datetime",
            date: feTodoDueDaysBefore,
            time: feTodoDueTimeOfDay,
          };
          break;
        default:
          return null;
      }
      fractionEncoding = { type: fractionEncodingType as "todo", due: todoDue };
      break;
    default:
      return null;
  }
  return {
    langCode,
    zipcodeId,
    streetId,
    houseNumber,
    filterAllEvents,
    filterAllFractions,
    filterSelectedFractions: filterAllFractions ? [] : filterSelectedFractions,
    dateRange,
    fractionEncoding,
  };
};

export const defaultFormInputs: Partial<FormInputs> = {
  langCode: LangCode.NL,
  dateRangeType: "relative",
  fractionEncodingType: "event",
  feEventStart: "07:00",
  feEventEnd: "10:00",
  reminders: [],
  feTodoDueType: "datetime",
  feTodoDueDaysBefore: 1,
  feTodoDueTimeOfDay: "20:00",
  filterAllEvents: true,
  filterAllFractions: true,
};

export const formToParams = ({
  langCode,
  zipcodeId,
  streetId,
  houseNumber,
  filterAllEvents,
  filterAllFractions,
  filterSelectedFractions,
  dateRange,
  fractionEncoding,
}: Form): URLSearchParams => {
  var params = new URLSearchParams({
    lc: langCode,
    z: zipcodeId,
    s: streetId,
    hn: houseNumber,
  });

  if (filterAllEvents) {
    params.append("fi", "e");
  }
  if (filterAllFractions) {
    params.append("fi", "f");
  }
  for (const f of filterSelectedFractions) {
    params.append("fif", f);
  }

  params.append("drt", dateRange.type);
  switch (dateRange.type) {
    case "absolute":
      params.append("f", dateRange.from);
      params.append("t", dateRange.to);
      break;
    case "relative":
      params.append("f", dateRange.from.toString());
      params.append("t", dateRange.to.toString());
      break;
  }

  params.append("fe", fractionEncoding.type);
  switch (fractionEncoding.type) {
    case "event":
      params.append("es", fractionEncoding.start);
      params.append("ee", fractionEncoding.end);
      for (const { rdb, rhb, rmb } of fractionEncoding.reminders) {
        params.append("rdb", rdb.toString());
        params.append("rhb", rhb.toString());
        params.append("rmb", rmb.toString());
      }
      break;
    case "todo":
      params.append("tdt", fractionEncoding.due.type);
      switch (fractionEncoding.due.type) {
        case "date":
          params.append("tdb", fractionEncoding.due.date.toString());
          break;
        case "datetime":
          params.append("tdb", fractionEncoding.due.date.toString());
          params.append("tt", fractionEncoding.due.time);
          break;
      }
      break;
  }
  return params;
};
