export enum LangCode {
  NL = "nl",
  FR = "fr",
  DE = "de",
  EN = "en",
}

export function LanguageSection({ register }: any) {
  return (
    <>
      <h3>Language</h3>
      <p>
        Choose in which language the waste collection titles and descriptions
        should be.
      </p>
      <fieldset>
        <legend>Language</legend>
        <label>
          <input
            type="radio"
            value={LangCode.NL}
            {...register("lc")}
            defaultChecked
          />
          Nederlands
        </label>
        <label>
          <input type="radio" value={LangCode.FR} {...register("lc")} />
          Francais
        </label>
        <label>
          <input type="radio" value={LangCode.DE} {...register("lc")} />
          Deutsch
        </label>
        <label>
          <input type="radio" value={LangCode.EN} {...register("lc")} />
          English
        </label>
      </fieldset>
    </>
  );
}
