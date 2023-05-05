import * as React from "react";
import { useFormContext, useWatch } from "react-hook-form";
import * as Api from "../api";

export default function LanguageSection() {
  const zipcode_id = useWatch({ name: "zipcode_id" });
  const street_id = useWatch({ name: "street_id" });
  const house_number = useWatch({ name: "house_number" });
  const selectedFractions = useWatch({ name: "fif" });
  const { register, setValue } = useFormContext();
  const [fractions, setFractions] = React.useState(null as any[] | null);
  React.useEffect(() => {
    if (zipcode_id && street_id && house_number) {
      Api.getFractions(zipcode_id, street_id, house_number).then(
        (fs: any[]) => {
          setFractions(fs);
          setValue(
            "fif",
            fs
              .map((f) => f.id)
              .filter((f) =>
                selectedFractions ? selectedFractions.includes(f) : true
              )
          );
        }
      );
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [zipcode_id, street_id, house_number, setValue]);
  return (
    <>
      <h3>Filter</h3>
      <p>Choose which fractions and events need to be included.</p>
      <fieldset>
        <legend>Filter</legend>
        <div>
          <label>
            <input type="checkbox" {...register("fi")} />
            All fractions
          </label>
          <p>Choose which fractions need to be included.</p>
          <fieldset>
            <legend>Fractions</legend>
            {fractions ? (
              fractions.map((fraction) => (
                <div key={fraction.id}>
                  <label>
                    <input
                      type="checkbox"
                      id={`fraction_${fraction.id}`}
                      value={fraction.id}
                      {...register("fif")}
                    />
                    {fraction.name.en}
                  </label>
                </div>
              ))
            ) : (
              <p>Please fill in your address first. </p>
            )}
          </fieldset>
        </div>
      </fieldset>
    </>
  );
}
