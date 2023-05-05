import { useForm, FormProvider } from "react-hook-form";
import * as React from "react";
import { LanguageSection } from "./section/language";
import AddressSection from "./section/address";
import FilterSection from "./section/filter";
import DateRangeSection from "./section/daterange";
import EncodingSection from "./section/encoding";
import DownloadSection from "./section/download";
import { FormInputs, defaultFormInputs } from "./types";

export function App() {
  const formContext = useForm<FormInputs>({
    defaultValues: defaultFormInputs,
  });

  return (
    <>
      <h1>Recycle ICS generator</h1>
      <p>Generate ICS files and links for your waste collections. </p>
      <FormProvider {...formContext}>
        <form>
          <h2>Generator form</h2>
          <LanguageSection />
          <AddressSection />
          <FilterSection />
          <DateRangeSection />
          <EncodingSection />
          <h2>Generate ICS</h2>
          <DownloadSection />
        </form>
      </FormProvider>
    </>
  );
}
