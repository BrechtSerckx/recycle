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
    { langCode: LangCode.FR, name: "Français" },
    { langCode: LangCode.DE, name: "Deutsch" },
    { langCode: LangCode.EN, name: "English" },
  ];
  return (
    <div className="card">
      <p className="card-label">Language</p>
      <p className="card-description">
        Language used for waste collection titles and descriptions.
      </p>
      <div className="lang-group">
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
      </div>
    </div>
  );
}
