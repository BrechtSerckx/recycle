import { useFormContext } from "react-hook-form";
import Autocompleter from "../Autocompleter";
import * as React from "react";
import { Inputs } from "../App";

const ZipcodeQueryInput = React.forwardRef((props: any, ref: any) => (
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
));

const ZipcodeAutocompleter = (props: any) => (
  <Autocompleter<string, string>
    fetchValues={(query) => ["foo", "bar", "baz"]}
    displayValue={(v) => <span>{v}</span>}
    {...props}
  />
);

const ZipcodeNameInput = React.forwardRef((props: any, ref: any) => (
  <label>
    City/town: <input ref={ref} type="text" readOnly {...props} />
  </label>
));

const ZipcodeIdInput = React.forwardRef((props: any, ref: any) => (
  <label>
    Zip code:
    <input ref={ref} type="text" readOnly {...props} />
  </label>
));

const StreetQueryInput = React.forwardRef((props: any, ref: any) => (
  <label>
    Search your street:
    <input ref={ref} type="text" placeholder="Grote Markt" {...props} />
  </label>
));

const StreetAutocompleter = (props: any) => (
  <Autocompleter<string, string>
    fetchValues={(query) => ["foo", "bar", "baz"]}
    displayValue={(v) => <span>{v}</span>}
    {...props}
  />
);

const StreetNameInput = React.forwardRef((props: any, ref: any) => (
  <label>
    Street name:
    <input ref={ref} type="text" readOnly {...props} />
  </label>
));

const StreetIdInput = React.forwardRef((props: any, ref: any) => (
  <label>
    Street ID:
    <input ref={ref} type="text" readOnly {...props} />
  </label>
));

const HouseNumberInput = React.forwardRef((props: any, ref: any) => (
  <label>
    House number:
    <input ref={ref} type="number" placeholder="1" {...props} />
  </label>
));

export default function AddressSection() {
  const [zipcodeSelected, setZipcodeSelected] = React.useState(false);
  const [streetSelected, setStreetSelected] = React.useState(false);
  const {
    register,
    watch,
    formState: { errors },
    setValue,
  } = useFormContext<Inputs>();
  return (
    <>
      <h3>Address</h3>
      <p>Waste collections are specific to your address.</p>
      <h4>Zip code</h4>
      <p>Search for your city and choose the correct city from the list.</p>
      <ZipcodeQueryInput
        {...register("zipcode_q", {
          onChange: () => setZipcodeSelected(false),
        })}
      />

      <ZipcodeAutocompleter
        query={watch("zipcode_q")}
        onSelect={(zipcode: string) => {
          setValue("zipcode_id", zipcode);
          setValue("zipcode_name", zipcode);
          setZipcodeSelected(true);
        }}
      />

      <ZipcodeNameInput {...register("zipcode_name", { required: true })} />
      <ZipcodeIdInput
        {...register("zipcode_id", {
          required: "Please select zip code",
        })}
      />
      {errors.zipcode_id && <span>{errors.zipcode_id.message}</span>}
      {zipcodeSelected && (
        <>
          <h4>Street</h4>
          <p>
            Search for your street and choose the correct street from the list.
          </p>
          <StreetQueryInput {...register("street_q")} />
          <StreetAutocompleter
            query={watch("street_q")}
            onSelect={(street: string) => {
              setValue("street_id", street);
              setValue("street_name", street);
              setStreetSelected(true);
            }}
          />
          <StreetNameInput {...register("street_name", { required: true })} />
          <StreetIdInput
            {...register("street_id", {
              required: "Please select street",
              onChange: () => setStreetSelected(false),
            })}
          />
          {errors.street_id && <span>{errors.street_id.message}</span>}
        </>
      )}
      {streetSelected && (
        <>
          <h4>House number</h4>
          <p>Enter your house number.</p>
          <HouseNumberInput {...register("house_number", { required: true })} />
        </>
      )}
    </>
  );
}
