import * as React from "react";
import { useFormContext, UseFormRegisterReturn } from "react-hook-form";
import { FormInputs } from "../types";

export enum LangCode {
  NL = "nl",
  FR = "fr",
  DE = "de",
  EN = "en",
}

const LangCodeRadio = React.forwardRef(
  (
    {
      children,
      ...props
    }: Partial<UseFormRegisterReturn> & {
      value: LangCode;
      children: React.ReactNode;
    },
    ref: React.ForwardedRef<HTMLInputElement>
  ) => (
    <label>
      <input ref={ref} type="radio" {...props} />
      {children}
    </label>
  )
);

export function LanguageSection() {
  const { register } = useFormContext<FormInputs>();
  const languages = [
    { langCode: LangCode.NL, name: "Nederlands" },
    { langCode: LangCode.FR, name: "Francais" },
    { langCode: LangCode.DE, name: "Deutsch" },
    { langCode: LangCode.EN, name: "English" },
  ];
  return (
    <>
      <h3>Language</h3>
      <p>
        Choose in which language the waste collection titles and descriptions
        should be.
      </p>
      <fieldset>
        <legend>Language</legend>
        {languages.map(({ langCode, name, ...props }) => (
          <LangCodeRadio
            key={langCode}
            value={langCode}
            {...props}
            {...register("langCode")}
          >
            {name}
          </LangCodeRadio>
        ))}
      </fieldset>
    </>
  );
}
