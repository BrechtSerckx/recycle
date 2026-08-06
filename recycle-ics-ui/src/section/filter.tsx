import * as React from "react";
import { useFormContext, useWatch } from "react-hook-form";
import * as Api from "../api";
import { friendlyError } from "../api";
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
  const [fractions, setFractions] = React.useState<any[] | null>(null);
  const [error, setError] = React.useState<string | null>(null);
  const [loading, setLoading] = React.useState(false);
  const lc = useWatch({ name: "langCode" });
  React.useEffect(() => {
    if (zipcodeId && streetId && houseNumber) {
      setError(null);
      setLoading(true);
      Api.getFractions(zipcodeId, streetId, houseNumber)
        .then((fs: any[]) => {
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
        })
        .catch((e: unknown) => {
          setFractions([]);
          setError(friendlyError(e));
        })
        .finally(() => setLoading(false));
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [zipcodeId, streetId, houseNumber, setValue]);

  return (
    <div className="card">
      <p className="card-label">Filter</p>
      <p className="card-description">
        Choose which fractions and events to include.
      </p>

      <div className="checkbox-group">
        <label>
          <input type="checkbox" {...register("filterAllEvents")} />
          All events
        </label>
        <label>
          <input
            type="checkbox"
            {...register("filterAllFractions", {
              onChange: (e: any) =>
                e.target.checked
                  ? setSelectedFractions((fractions ?? []).map((f) => f.id))
                  : setSelectedFractions([]),
            })}
          />
          All fractions
        </label>
      </div>

      <p className="card-section-title" style={{ marginTop: "1rem" }}>
        Fractions
      </p>

      {loading && <p className="msg-loading">Loading…</p>}
      {error && <p className="msg-error">{error}</p>}
      {fractions === null && !loading ? (
        <p className="msg-empty">Fill in your address above to load fractions.</p>
      ) : !loading && fractions !== null && fractions.length === 0 && !error ? (
        <p className="msg-empty">No fractions found for this address.</p>
      ) : (
        <div className="checkbox-group" style={{ marginTop: "0.25rem" }}>
          {(fractions ?? []).map((fraction) => (
            <label key={fraction.id}>
              <input
                type="checkbox"
                value={fraction.id}
                {...register("filterSelectedFractions", {
                  onChange: (e: any) => {
                    if (e.target.checked) {
                      if (
                        (fractions ?? []).every(
                          (f) =>
                            f.id === e.target.value ||
                            selectedFractions.includes(f.id)
                        )
                      ) {
                        setAllFractions(true);
                      }
                    } else {
                      if (
                        (fractions ?? []).every((f) =>
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
          ))}
        </div>
      )}
    </div>
  );
}
