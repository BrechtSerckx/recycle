import { useForm, FormProvider, SubmitHandler } from "react-hook-form";
import * as React from "react";
import { LangCode, LanguageSection } from "./section/language";
import AddressSection from "./section/address";
import DateRangeSection from "./section/daterange";
import EncodingSection from "./section/encoding";

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
  reminders: [{ rdb: number; rhb: number; rmb: number }];
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
        reminders: [{ rdb: number; rhb: number; rmb: number }];
      }
    | {
        fe: "todo";
        todo_due:
          | { tdt: "date"; tdb: number }
          | { tdt: "datetime"; tdb: number; tt: string };
      };
};
const inputsToForm = ({
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
}: FormInputs): Form => {
  var date_range;
  switch (drt) {
    case "absolute":
      date_range = { drt: drt as "absolute", f: adrf, t: adrt };
      break;
    case "relative":
      date_range = { drt: drt as "relative", f: rdrf, t: rdrt };
      break;
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
      }
      fraction_encoding = { fe: fe as "todo", todo_due };
      break;
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

export function App() {
  const formContext = useForm<FormInputs>();
  const { handleSubmit } = formContext;
  const [values, setValues] = React.useState<Form | null>(null);
  const onSubmit: SubmitHandler<FormInputs> = (data) => {
    const formData = inputsToForm(data);
    console.log(formData);
    setValues(formData);
  };

  return (
    <>
      <h1>Recycle ICS generator</h1>
      <p>Generate ICS files and links for your waste collections. </p>
      <FormProvider {...formContext}>
        <form onSubmit={handleSubmit(onSubmit)}>
          <h2>Generator form</h2>
          <LanguageSection />
          <AddressSection />
          <DateRangeSection />
          <EncodingSection />

          <h3>Submit</h3>

          <input type="submit" />
          <h2>Form submit</h2>
          <p>{values && JSON.stringify(values)}</p>
        </form>
      </FormProvider>
    </>
  );
}
