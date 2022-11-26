import * as React from "react";

export default function Autocompleter<Q, V>({
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
