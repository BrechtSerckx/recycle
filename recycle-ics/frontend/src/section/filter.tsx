import * as React from "react";
import { useFormContext, useWatch } from "react-hook-form";
import * as Api from "../api";

export default function LanguageSection() {
  const zipcode_id = useWatch({ name: "zipcode_id" }),
    street_id = useWatch({ name: "street_id" }),
    house_number = useWatch({ name: "house_number" });
  const selectedFractions = useWatch({ name: "fif" }),
    setSelectedFractions = (fs: string[]) => setValue("fif", fs);
  const allFractions = useWatch({ name: "fi" }),
    setAllFractions = (b: boolean) => setValue("fi", b);
  const { register, setValue } = useFormContext();
  const [fractions, setFractions] = React.useState([] as any[]);
  React.useEffect(() => {
    if (zipcode_id && street_id && house_number) {
      Api.getFractions(zipcode_id, street_id, house_number).then(
        (fs: any[]) => {
          const oldFractions = fractions || [];
          setFractions(fs);
          setValue(
            "fif",
            fs
              .map((f) => f.id)
              .filter((f) =>
                oldFractions.map((v) => v.id).includes(f)
                  ? selectedFractions
                    ? selectedFractions.includes(f)
                    : allFractions
                  : allFractions
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
            <input
              type="checkbox"
              {...register("fi", {
                onChange: (e: any) =>
                  e.target.checked
                    ? setSelectedFractions(fractions.map((f) => f.id))
                    : setSelectedFractions([]),
              })}
            />
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
                      {...register("fif", {
                        onChange: (e: any) => {
                          if (e.target.checked) {
                            if (
                              fractions.every(
                                (f) =>
                                  f.id === e.target.value ||
                                  selectedFractions.includes(f.id)
                              )
                            ) {
                              setAllFractions(true);
                            }
                          } else {
                            if (
                              fractions.every((f) =>
                                selectedFractions.includes(f.id)
                              )
                            ) {
                              setAllFractions(false);
                            }
                          }
                        },
                      })}
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
