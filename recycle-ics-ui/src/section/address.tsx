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
      Search your zip code
      <input ref={ref} type="text" inputMode="numeric" placeholder="3000" {...props} />
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
    <div className="checkbox-group" style={{ marginTop: "0.25rem" }}>
      {loading && <p className="msg-loading">Loading…</p>}
      {error && <p className="msg-error">{error}</p>}
      {!loading && values !== null && values.length === 0 && (
        <p className="msg-empty">No zip codes found for "{query}".</p>
      )}
      {values &&
        values.map((v) => (
          <label key={v.id}>
            <input
              type="radio"
              value={v.id}
              disabled={!v.available}
              {...register("zipcodeId")}
            />
            <span>
              {v.code} {(v.names[0] || v.city.names)[lc]}, {v.city.names[lc]}
              {v.available || " (unavailable)"}
            </span>
          </label>
        ))}
    </div>
  );
};

const StreetQueryInput = React.forwardRef(
  (
    props: Partial<UseFormRegisterReturn>,
    ref: React.ForwardedRef<HTMLInputElement>
  ) => (
    <label>
      Search your street
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
    <div className="checkbox-group" style={{ marginTop: "0.25rem" }}>
      {loading && <p className="msg-loading">Loading…</p>}
      {error && <p className="msg-error">{error}</p>}
      {!loading && values !== null && values.length === 0 && (
        <p className="msg-empty">No streets found for "{query}".</p>
      )}
      {values &&
        values.map((v) => (
          <label key={v.id}>
            <input type="radio" value={v.id} {...register("streetId")} />
            {v.names[lc]}
          </label>
        ))}
    </div>
  );
};

const HouseNumberInput = React.forwardRef(
  (
    props: Partial<UseFormRegisterReturn>,
    ref: React.ForwardedRef<HTMLInputElement>
  ) => (
    <label>
      House number
      <input ref={ref} type="number" placeholder="1" min={1} {...props} />
    </label>
  )
);

export default function AddressSection() {
  const { register } = useFormContext<FormInputs>();
  const zipcodeId = useWatch({ name: "zipcodeId" });
  const streetId = useWatch({ name: "streetId" });
  return (
    <div className="card">
      <p className="card-label">Address</p>
      <p className="card-description">
        Waste collections are specific to your address.
      </p>

      <p className="card-section-title">Zip code</p>
      <ZipcodeQueryInput {...register("zipcodeQuery")} />
      <ZipcodeAutocompleter />

      {zipcodeId && (
        <>
          <p className="card-section-title">Street</p>
          <StreetQueryInput {...register("streetQuery")} />
          <StreetAutocompleter zipcode={zipcodeId} />
        </>
      )}

      {streetId && (
        <>
          <p className="card-section-title">House number</p>
          <HouseNumberInput {...register("houseNumber", { required: true })} />
        </>
      )}
    </div>
  );
}
