import { useForm, SubmitHandler } from "react-hook-form";
import * as React from "react";
import { LangCode, LanguageSection } from "./section/language";
import AddressSection from "./section/address";
import DateRangeSection from "./section/daterange";
import EncodingSection from "./section/encoding";

type Inputs = {
  example: string;
  exampleRequired: string;
  lc: LangCode;
  zipcode_q: string;
  zipcode_id: string;
  zipcode_name: string;
  street_q: string;
  street_id: string;
  street_name: string;
  house_number: string;
};

export default function App() {
  const {
    register,
    handleSubmit,
    watch,
    formState: { errors },
    setValue,
  } = useForm<Inputs>({ shouldFocusError: false });
  const onSubmit: SubmitHandler<Inputs> = (data) => console.log(data);

  return (
    <>
      <h1>Recycle ICS generator</h1>
      <p>Generate ICS files and links for your waste collections. </p>
      <form onSubmit={handleSubmit(onSubmit)}>
        <h2>Generator form</h2>
        <LanguageSection register={register} />
        <AddressSection
          register={register}
          watch={watch}
          errors={errors}
          setValue={setValue}
        />
        <DateRangeSection />
        <EncodingSection />

        <h3>Submit</h3>

        <input type="submit" />
      </form>
    </>
  );
}
