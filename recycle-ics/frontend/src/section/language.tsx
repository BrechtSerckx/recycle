import * as React from "react";
import { useFormContext } from "react-hook-form";

export enum LangCode {
  NL = "nl",
  FR = "fr",
  DE = "de",
  EN = "en",
}

const LangCodeRadio = React.forwardRef(
  ({ children, ...props }: any, ref: any) => (
    <label>
      <input ref={ref} type="radio" {...props} />
      {children}
    </label>
  )
);

export function LanguageSection() {
  const { register } = useFormContext();
  const languages = [
    { lc: LangCode.NL, name: "Nederlands" },
    { lc: LangCode.FR, name: "Francais" },
    { lc: LangCode.DE, name: "Deutsch" },
    { lc: LangCode.EN, name: "English" },
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
        {languages.map(({ lc, name, ...props }) => (
          <LangCodeRadio key={lc} value={lc} {...props} {...register("lc")}>
            {name}
          </LangCodeRadio>
        ))}
      </fieldset>
    </>
  );
}
