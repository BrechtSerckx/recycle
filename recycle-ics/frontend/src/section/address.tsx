import Autocompleter from "../Autocompleter";
import * as React from "react";

export default function AddressSection({
  register,
  setValue,
  errors,
  watch,
}: any) {
  const [zipcodeSelected, setZipcodeSelected] = React.useState(false);
  const [streetSelected, setStreetSelected] = React.useState(false);
  return (
    <>
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
          {...register("zipcode_q", {
            onChange: () => setZipcodeSelected(false),
          })}
        />
      </label>
      <Autocompleter<string, string>
        query={watch("zipcode_q")}
        fetchValues={(query) => ["foo", "bar", "baz"]}
        onSelect={(zipcode) => {
          setValue("zipcode_id", zipcode);
          setValue("zipcode_name", zipcode);
          setZipcodeSelected(true);
        }}
        displayValue={(v) => <span>{v}</span>}
      />
      {zipcodeSelected && (
        <>
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

          <h4>Street</h4>

          <div>
            <p>
              Search for your street and choose the correct street from the
              list.
            </p>
          </div>
          <label>
            Search your street:
            <input
              type="text"
              placeholder="Grote Markt"
              {...register("street_q")}
            />
          </label>
          <Autocompleter<string, string>
            query={watch("street_q")}
            fetchValues={(query) => ["foo", "bar", "baz"]}
            onSelect={(street) => {
              setValue("street_id", street);
              setValue("street_name", street);
              setStreetSelected(true);
            }}
            displayValue={(v) => <span>{v}</span>}
          />
          {streetSelected && (
            <>
              <label>
                Street name:
                <input
                  type="text"
                  readOnly
                  {...register("street_name", { required: true })}
                />
              </label>
              <label>
                Street ID:
                <input
                  type="text"
                  readOnly
                  {...register("street_id", {
                    required: "Please select street",
                    onChange: () => setStreetSelected(false),
                  })}
                />
                {errors.street_id && <span>{errors.street_id.message}</span>}
              </label>

              <div>
                <h4>House number</h4>
                <p>Enter your house number.</p>
                <label>
                  House number:
                  <input
                    type="number"
                    placeholder="1"
                    {...register("house_number", { required: true })}
                  />
                </label>
              </div>
            </>
          )}
        </>
      )}
    </>
  );
}
