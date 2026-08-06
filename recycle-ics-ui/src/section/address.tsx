import {
  useWatch,
  useFormContext,
  UseFormRegisterReturn,
} from "react-hook-form";
import { debounce } from "../Autocompleter";
import * as React from "react";
import { FormInputs } from "../types";
import * as Api from "../api";
import { friendlyError } from "../api";

const ZipcodeQueryInput = React.forwardRef(
  (
    props: Partial<UseFormRegisterReturn>,
    ref: React.ForwardedRef<HTMLInputElement>
  ) => (
    <label>
      Search your zip code:
      <input
        ref={ref}
        type="text"
        inputMode="numeric"
        placeholder="3000"
        {...props}
      />
    </label>
  )
);

const ZipcodeAutocompleter = (props: Partial<UseFormRegisterReturn>) => {
  const lc = useWatch({ name: "langCode" });
  const query = useWatch({ name: "zipcodeQuery" });
  const { register, setValue } = useFormContext<FormInputs>();
  const [values, setValues] = React.useState<any[] | null>(null);
  const [error, setError] = React.useState<string | null>(null);
  const [loading, setLoading] = React.useState(false);
  React.useEffect(() => {
    setValue("zipcodeId", null);
    setValues(null);
    setError(null);
    setLoading(false);
    if (query) {
      debounce(() => {
        if (query.length >= 2) {
          setLoading(true);
          Api.searchZipcodes(query)
            .then((newValues) => setValues(newValues))
            .catch((e: unknown) => setError(friendlyError(e)))
            .finally(() => setLoading(false));
        }
      }, 250)();
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [query]);
  return (
    <fieldset>
      <legend>Zip code</legend>
      {loading && <p>Loading…</p>}
      {error && <p style={{ color: "red" }}>{error}</p>}
      {!loading && values !== null && values.length === 0 && (
        <p>No zip codes found for "{query}".</p>
      )}
      {values &&
        values.map((v) => (
          <div key={v.id}>
            <label>
              <input
                type="radio"
                value={v.id}
                disabled={!v.available}
                {...register("zipcodeId")}
              />
              <span>
                {v.code} {(v.names[0] || v.city.names)[lc]}, {v.city.names[lc]}
                {v.available || " (Unavailable)"}
              </span>
            </label>
          </div>
        ))}
    </fieldset>
  );
};

const StreetQueryInput = React.forwardRef(
  (
    props: Partial<UseFormRegisterReturn>,
    ref: React.ForwardedRef<HTMLInputElement>
  ) => (
    <label>
      Search your street:
      <input ref={ref} type="text" placeholder="Grote Markt" {...props} />
    </label>
  )
);

const StreetAutocompleter = ({
  zipcode,
  ...props
}: Partial<UseFormRegisterReturn> & { zipcode: string }) => {
  const lc = useWatch({ name: "langCode" });
  const query = useWatch({ name: "streetQuery" });
  const { register, setValue } = useFormContext<FormInputs>();
  const [values, setValues] = React.useState<any[] | null>(null);
  const [error, setError] = React.useState<string | null>(null);
  const [loading, setLoading] = React.useState(false);
  React.useEffect(() => {
    setValue("streetId", null);
    setValues(null);
    setError(null);
    setLoading(false);
    if (query) {
      debounce(() => {
        if (query.length >= 3) {
          setLoading(true);
          Api.searchStreets(zipcode, query)
            .then((newValues) => setValues(newValues))
            .catch((e: unknown) => setError(friendlyError(e)))
            .finally(() => setLoading(false));
        }
      }, 250)();
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [query, zipcode]);
  return (
    <fieldset>
      <legend>Street</legend>
      {loading && <p>Loading…</p>}
      {error && <p style={{ color: "red" }}>{error}</p>}
      {!loading && values !== null && values.length === 0 && (
        <p>No streets found for "{query}".</p>
      )}
      {values &&
        values.map((v) => (
          <div key={v.id}>
            <label>
              <input type="radio" value={v.id} {...register("streetId")} />
              {v.names[lc]}
            </label>
          </div>
        ))}
    </fieldset>
  );
};

const HouseNumberInput = React.forwardRef(
  (
    props: Partial<UseFormRegisterReturn>,
    ref: React.ForwardedRef<HTMLInputElement>
  ) => (
    <label>
      House number:
      <input ref={ref} type="number" placeholder="1" min={1} {...props} />
    </label>
  )
);

export default function AddressSection() {
  const { register } = useFormContext<FormInputs>();
  const zipcodeId = useWatch({ name: "zipcodeId" });
  const streetId = useWatch({ name: "streetId" });
  return (
    <>
      <h3>Address</h3>
      <p>Waste collections are specific to your address.</p>
      <h4>Zip code</h4>
      <p>Search for your city and choose the correct city from the list.</p>
      <ZipcodeQueryInput {...register("zipcodeQuery")} />
      <ZipcodeAutocompleter />
      {zipcodeId && (
        <>
          <h4>Street</h4>
          <p>
            Search for your street and choose the correct street from the list.
          </p>
          <StreetQueryInput {...register("streetQuery")} />
          <StreetAutocompleter zipcode={zipcodeId} />
        </>
      )}
      {streetId && (
        <>
          <h4>House number</h4>
          <p>Enter your house number.</p>
          <HouseNumberInput {...register("houseNumber", { required: true })} />
        </>
      )}
    </>
  );
}
