import * as React from "react";

export default function Autocompleter<V>({
  query,
  fetchValues,
  onSelect,
  displayValue,
}: {
  query: string;
  fetchValues: (q: string) => Promise<V[]>;
  onSelect: (v: V) => any;
  displayValue: (v: V) => React.ReactNode;
}) {
  const [values, setValues] = React.useState<V[]>([]);
  React.useEffect(() => {
    if (query) {
      debounce(() => {
        if (query.length >= 3) {
          fetchValues(query).then((newValues) => setValues(newValues));
        }
      }, 250)();
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [query]);
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

// Returns a function, that, as long as it continues to be invoked, will not
// be triggered. The function will be called after it stops being called for
// N milliseconds. If `immediate` is passed, trigger the function on the
// leading edge, instead of the trailing.
//
// See: https://stackoverflow.com/questions/24004791/can-someone-explain-the-debounce-function-in-javascript
function debounce(func: () => void, wait: number): () => void {
  // 'private' variable for instance
  // The returned function will be able to reference this due to closure.
  // Each call to the returned function will share this common timer.
  var timeout: ReturnType<typeof setTimeout>;

  // Calling debounce returns a new anonymous function
  return () => {
    // This is the basic debounce behaviour where you can call this
    //   function several times, but it will only execute once
    //   [before or after imposing a delay].
    //   Each time the returned function is called, the timer starts over.
    clearTimeout(timeout);

    // Set the new timeout
    timeout = setTimeout(function () {
      // Call the original function with apply
      // apply lets you define the 'this' object as well as the arguments
      //    (both captured before setTimeout)
      func();
    }, wait);
  };
}
