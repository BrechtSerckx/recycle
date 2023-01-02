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
};
export type Form = {
  lc: LangCode;
  zipcode_id: string;
  street_id: string;
  house_number: string;
  date_range:
    | { drt: "absolute"; f: string; t: string }
    | { drt: "relative"; f: number; t: number };
};
const inputsToForm = ({
  drt,
  adrf,
  adrt,
  rdrf,
  rdrt,
  ...inputs
}: FormInputs): Form => {
  var date_range;
  switch (drt) {
    case "absolute":
      date_range = { drt: "absolute" as "absolute", f: adrf, t: adrt };
      break;
    case "relative":
      date_range = { drt: "relative" as "relative", f: rdrf, t: rdrt };
      break;
  }
  return { date_range, ...inputs };
};
};

export function App() {
  const formContext = useForm<Inputs>();
  const { handleSubmit } = formContext;
  const onSubmit: SubmitHandler<Inputs> = (data) => console.log(data);

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
        </form>
      </FormProvider>
    </>
  );
}
