import { useForm, SubmitHandler } from "react-hook-form";
import * as React from "react";

enum LangCode {
  NL = "nl",
  FR = "fr",
  DE = "de",
  EN = "en",
}

type Inputs = {
  example: string;
  exampleRequired: string;
  lc: LangCode;
  zipcode_q: string;
  zipcode_id: string;
  zipcode_name: string;
};

function Autocompleter<Q, V>({
  query,
  fetchValues,
  onSelect,
  displayValue,
}: {
  query: Q;
  fetchValues: (q: Q) => V[];
  onSelect: (v: V) => any;
  displayValue: (v: V) => React.ReactNode;
}) {
  const [values, setValues] = React.useState<V[]>([]);
  React.useEffect(() => {
    if (query) {
      console.log(`Autocompleting ${query}`);
      setValues(fetchValues(query));
    }
  }, [query, fetchValues]);
  return (
    <ul>
      {values.map((value, index) => (
        <li key={index} onClick={() => onSelect(value)}>
          {displayValue(value)}
        </li>
      ))}
    </ul>
  );
}

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

        <h3>Address</h3>
        <p>Waste collections are specific to your address.</p>
        <h4>Zip code</h4>
        <div>
          <p>Search for your city and choose the correct city from the list.</p>
        </div>

        <label>
          Search your zip code:
          <input
            type="text"
            inputMode="numeric"
            placeholder="3000"
            {...register("zipcode_q")}
          />
        </label>
        <Autocompleter<string, string>
          query={watch("zipcode_q")}
          fetchValues={(query) => ["foo", "bar", "baz"]}
          onSelect={(zipcode) => {
            setValue("zipcode_id", zipcode);
            setValue("zipcode_name", zipcode);
          }}
          displayValue={(v) => <span>{v}</span>}
        />
        <label>
          City/town:
          <input
            type="text"
            readOnly
            {...register("zipcode_name", { required: true })}
          />
        </label>
        <label>
          Zip code:
          <input
            type="text"
            readOnly
            {...register("zipcode_id", {
              required: "Please select zip code",
            })}
          />
          {errors.zipcode_id && <span>{errors.zipcode_id.message}</span>}
        </label>

        <input type="submit" />
      </form>
    </>
  );
}
