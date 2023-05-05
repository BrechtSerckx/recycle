import { useFormContext, UseFormRegisterReturn } from "react-hook-form";
import Autocompleter from "../Autocompleter";
import * as React from "react";
import { FormInputs } from "../types";
import * as Api from "../api";

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

const ZipcodeAutocompleter = (
  props: Partial<UseFormRegisterReturn> & {
    query: string;
    onSelect: (v: string) => any;
  }
) => (
  <Autocompleter<any>
    fetchValues={(query) => Api.searchZipcodes(query)}
    displayValue={(v) => (
      <span>
        {v.city.name} ({v.code})
      </span>
    )}
    {...props}
  />
);

const ZipcodeNameInput = React.forwardRef(
  (
    props: Partial<UseFormRegisterReturn>,
    ref: React.ForwardedRef<HTMLInputElement>
  ) => (
    <label>
      City/town: <input ref={ref} type="text" readOnly {...props} />
    </label>
  )
);

const ZipcodeIdInput = React.forwardRef(
  (
    props: Partial<UseFormRegisterReturn>,
    ref: React.ForwardedRef<HTMLInputElement>
  ) => (
    <label>
      Zip code:
      <input ref={ref} type="text" readOnly {...props} />
    </label>
  )
);

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
}: Partial<UseFormRegisterReturn> & {
  zipcode: string;
  query: string;
  onSelect: (v: string) => any;
}) => (
  <Autocompleter<any>
    fetchValues={(query) => Api.searchStreets(zipcode, query)}
    displayValue={(v) => <span>{v.name} (v.id)</span>}
    {...props}
  />
);

const StreetNameInput = React.forwardRef(
  (
    props: Partial<UseFormRegisterReturn>,
    ref: React.ForwardedRef<HTMLInputElement>
  ) => (
    <label>
      Street name:
      <input ref={ref} type="text" readOnly {...props} />
    </label>
  )
);

const StreetIdInput = React.forwardRef(
  (
    props: Partial<UseFormRegisterReturn>,
    ref: React.ForwardedRef<HTMLInputElement>
  ) => (
    <label>
      Street ID:
      <input ref={ref} type="text" readOnly {...props} />
    </label>
  )
);

const HouseNumberInput = React.forwardRef(
  (
    props: Partial<UseFormRegisterReturn>,
    ref: React.ForwardedRef<HTMLInputElement>
  ) => (
    <label>
      House number:
      <input ref={ref} type="number" placeholder="1" {...props} />
    </label>
  )
);

export default function AddressSection() {
  const [zipcodeSelected, setZipcodeSelected] = React.useState(false);
  const [streetSelected, setStreetSelected] = React.useState(false);
  const {
    register,
    watch,
    formState: { errors },
    setValue,
  } = useFormContext<FormInputs>();
  const resetStreet = () => {
    setValue("street_id", "");
    setValue("street_name", "");
    setStreetSelected(false);
  };
  return (
    <>
      <h3>Address</h3>
      <p>Waste collections are specific to your address.</p>
      <h4>Zip code</h4>
      <p>Search for your city and choose the correct city from the list.</p>
      <ZipcodeQueryInput {...register("zipcode_q")} />

      <ZipcodeAutocompleter
        query={watch("zipcode_q")}
        onSelect={(zipcode: any) => {
          setValue("zipcode_id", zipcode.id);
          setValue("zipcode_name", zipcode.city.name);
          setZipcodeSelected(true);
          resetStreet();
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
            zipcode={watch("zipcode_id")}
            query={watch("street_q")}
            onSelect={(street: any) => {
              setValue("street_id", street.id);
              setValue("street_name", street.name);
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
