export enum LangCode {
  NL = "nl",
  FR = "fr",
  DE = "de",
  EN = "en",
}

export function LanguageSection({ lcProps }: { lcProps: any }) {
  const LangCodeRadio = ({ children, ...props }: any) => {
    return (
      <label>
        <input type="radio" {...props} {...lcProps} />
        {children}
      </label>
    );
  };
  return (
    <>
      <h3>Language</h3>
      <p>
        Choose in which language the waste collection titles and descriptions
        should be.
      </p>
      <fieldset>
        <legend>Language</legend>
        <LangCodeRadio value={LangCode.NL} defaultChecked>
          Nederlands
        </LangCodeRadio>
        <LangCodeRadio value={LangCode.FR}>Francais</LangCodeRadio>
        <LangCodeRadio value={LangCode.DE}>Deutsch</LangCodeRadio>
        <LangCodeRadio value={LangCode.EN}>English</LangCodeRadio>
      </fieldset>
    </>
  );
}
