// Returns a "trigger" function that, as long as it continues to be invoked,
// will not call the function it's passed. The function is called after the
// trigger stops being invoked for `wait` milliseconds.
//
// Call `debounce(wait)` ONCE to get a trigger that shares a single `timeout`
// across all its calls (e.g. via `React.useRef`), then call that trigger
// repeatedly with the latest callback. Calling `debounce(wait)` again for
// each callback would give each call its own private `timeout`, defeating
// the debounce.
//
// See: https://stackoverflow.com/questions/24004791/can-someone-explain-the-debounce-function-in-javascript
export function debounce(wait: number): (func: () => void) => void {
  let timeout: ReturnType<typeof setTimeout>;
  return (func) => {
    clearTimeout(timeout);
    timeout = setTimeout(func, wait);
  };
}
