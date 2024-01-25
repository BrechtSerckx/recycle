import * as React from "react";
import { useFormContext, useWatch } from "react-hook-form";
import * as Api from "../api";
import { FormInputs } from "../types";

export default function FilterSection() {
  const zipcodeId = useWatch({ name: "zipcodeId" }),
    streetId = useWatch({ name: "streetId" }),
    houseNumber = useWatch({ name: "houseNumber" });
  const selectedFractions = useWatch({ name: "filterSelectedFractions" }),
    setSelectedFractions = (fs: string[]) =>
      setValue("filterSelectedFractions", fs);
  const allFractions = useWatch({ name: "filterAllFractions" }),
    setAllFractions = (b: boolean) => setValue("filterAllFractions", b);
  const { register, setValue } = useFormContext<FormInputs>();
  const [fractions, setFractions] = React.useState([] as any[]);
  const lc = useWatch({ name: "langCode" });
  React.useEffect(() => {
    if (zipcodeId && streetId && houseNumber) {
      Api.getFractions(zipcodeId, streetId, houseNumber).then((fs: any[]) => {
        const oldFractions = fractions || [];
        setFractions(fs);
        setValue(
          "filterSelectedFractions",
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
      });
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [zipcodeId, streetId, houseNumber, setValue]);
  return (
    <>
      <h3>Filter</h3>
      <p>Choose which fractions and events need to be included.</p>
      <fieldset>
        <legend>Filter</legend>
        <div>
          <div>
            <label>
              <input type="checkbox" {...register("filterAllEvents")} />
              All events
            </label>
          </div>
          <div>
            <label>
              <input
                type="checkbox"
                {...register("filterAllFractions", {
                  onChange: (e: any) =>
                    e.target.checked
                      ? setSelectedFractions(fractions.map((f) => f.id))
                      : setSelectedFractions([]),
                })}
              />
              All fractions
            </label>
          </div>
          <p>Choose which fractions need to be included.</p>
          <fieldset>
            <legend>Fractions</legend>
            {fractions ? (
              fractions.map((fraction) => (
                <div key={fraction.id}>
                  <label>
                    <input
                      type="checkbox"
                      value={fraction.id}
                      {...register("filterSelectedFractions", {
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
                    {fraction.name[lc]}
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
