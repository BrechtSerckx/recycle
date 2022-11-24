// Attach a custom submit function to the form.
function attachFormSubmit(form, permalink, openLink, downloadLink) {

    // Function to execute when the form is submitted.
    function submit() {
        // get form data
        const formData = new FormData(form);
        for (var entry of formData.entries()) {
            console.log(entry[0], entry[1]);
        }

        // make url
        function mkUrl(base) {
            var url = new URL('/api/generate', base);
            url.search = new URLSearchParams(formData).toString();
            return url;
        }

        // set permalink
        permalink.value = mkUrl("webcal://" + window.location.host);

        // set open link
        openLink.disabled = false;
        openLink.href = mkUrl("webcal://" + window.location.host);

        // set download link
        downloadLink.disabled = false;
        downloadLink.href = mkUrl(window.location.origin);

        // show permalink and download link
        const link = document.getElementById("link");
        link.style.display = "block";
        link.scrollIntoView();
    }

    form.addEventListener("submit", (event) => {
        event.preventDefault();
        submit();
    });
}

// Returns a function, that, as long as it continues to be invoked, will not
// be triggered. The function will be called after it stops being called for
// N milliseconds. If `immediate` is passed, trigger the function on the
// leading edge, instead of the trailing.
//
// See: https://stackoverflow.com/questions/24004791/can-someone-explain-the-debounce-function-in-javascript
function debounce(func, wait) {
    // 'private' variable for instance
    // The returned function will be able to reference this due to closure.
    // Each call to the returned function will share this common timer.
    var timeout;

    // Calling debounce returns a new anonymous function
    return function() {
        // reference the context and args for the setTimeout function
        var context = this,
            args = arguments;

        // This is the basic debounce behaviour where you can call this 
        //   function several times, but it will only execute once 
        //   [before or after imposing a delay]. 
        //   Each time the returned function is called, the timer starts over.
        clearTimeout(timeout);

        // Set the new timeout
        timeout = setTimeout(function() {

            // Inside the timeout function, clear the timeout variable
            // which will let the next execution run when in 'immediate' mode
            timeout = null;
            // Call the original function with apply
            // apply lets you define the 'this' object as well as the arguments 
            //    (both captured before setTimeout)
            func.apply(context, args);
        }, wait);
    };
}

function getLangCode() {
    var checkedRadio = Array.from(document.querySelectorAll("[name=lc]")).filter(x => x.checked)[0];
    console.log(checkedRadio.value);
    return checkedRadio.value;
}

// Enable a radio input and its corresponding sub-inputs.
function enableRadioSubInputs(radios, thisRadio, f = () => {}) {
    thisRadio.checked = true;
    // for all other radio inputs, disable the corresponding inputs
    for (thatRadio of radios) {
        if (thatRadio.id != thisRadio.id) {

            Array.from(document.getElementsByClassName(thatRadio.id)).forEach(input =>
                input.disabled = true
            );
        } else {}
    }
    // enable the corresponding inputs for this radio
    Array.from(document.getElementsByClassName(thisRadio.id)).forEach(input =>
        input.disabled = false
    );
    // callback
    f(thisRadio);
}

// Attach a function to the `date_range_type` radio inputs so their respective
// input fields get enabled or disabled based on the radio input.
function attachRadioSwitch(radios, f = () => {}) {
    for (const radio of radios) {
        radio.onclick = () => {
            enableRadioSubInputs(radios, radio, f);
        };
    }
}


function searchZipcodes(q) {
    return fetch(`/api/search-zipcode?q=${q}`)
        .then(response => response.json());
}

function searchStreets(zipcode, q) {
    return fetch(`/api/search-street?zipcode=${zipcode}&q=${q}`)
        .then(response => response.json());
}

function getFractions(zipcode, street, house_number) {
    return fetch(`/api/fractions?zipcode=${zipcode}&street=${street}&house_number=${house_number}`)
        .then(response => response.json());
}

function removeChildren(node) {
    while (node.firstChild) {
        node.removeChild(node.lastChild);
    }
}

function createAutoCompleter(input, container, getData, mkLabel, withResult, threshold = 3) {
    input.oninput = debounce(function() {
        const q = this.value;
        removeChildren(container);
        if (q.length >= threshold) {
            getData(q).then(results => {
                for (i = 0; i < results.length; i++) {
                    const result = results[i];
                    const node = document.createElement("li");
                    const label = mkLabel(result);
                    const labelNode = document.createTextNode(label);
                    node.appendChild(labelNode);
                    node.onclick = () => {
                        input.value = label;
                        removeChildren(container);
                        withResult(result);
                    };
                    container.appendChild(node);
                }
            });
        }
    }, 250);
};

function createZipcodeCompleter(input, container, target, streetQInput, submitButton) {
    createAutoCompleter(input,
        container,
        q => searchZipcodes(q),
        res => {
            let lc = getLangCode();
                cityName = res.city.names[lc];
                zipcode = res.code;
                names = res.names.map(n => n[lc]);
            return `${cityName} (${zipcode}) - ${names.join(', ')}`;
        },
        res => {
            target.value = res.id;
            streetQInput.value = "";
            streetQInput.disabled = false;
            submitButton.disabled = true;
        }
    );
}

function createStreetCompleter(input, container, target, zipcodeInput, houseNumberInput, submitButton) {
    createAutoCompleter(input,
        container,
        q => searchStreets(zipcodeInput.value, q),
        res => {
            const lc = getLangCode();
            return res.names[lc];
        },
        res => {
            target.value = res.id;
            houseNumberInput.disabled = false;
        }
    );
}

function createEditableList(reminders, prototype, addReminderButton) {
    function deleteReminder(reminder) {
        reminder.parentNode.removeChild(reminder);
    }

    function addReminder() {
        const newReminder = prototype.cloneNode(true);
        newReminder.style.display = "list-item";
        Array.from(newReminder.getElementsByTagName("input")).forEach(input => input.disabled = false);
        newReminder.querySelector("#delete_reminder_button").onclick = () => deleteReminder(newReminder);
        reminder_list.appendChild(newReminder);
    }
    addReminderButton.onclick = addReminder;
}

function main() {
    attachFormSubmit(document.getElementById("recycleForm"),
        document.getElementById("permalink"),
        document.getElementById("openLink"),
        document.getElementById("downloadLink")
    );

    const dateRangeRadios = document.getElementsByName("drt");
    attachRadioSwitch(dateRangeRadios);
    enableRadioSubInputs(dateRangeRadios, document.getElementById("date_range_type_rel"));

    const todoDueRadios = document.getElementsByName("tdt");
    attachRadioSwitch(todoDueRadios);
    enableRadioSubInputs(todoDueRadios, document.getElementById("todo_due_type_datetime"));

    const fractionEncodingRadios = document.getElementsByName("fe");

    function enableActiveTodoDueType(thisRadio) {
        if (thisRadio == document.getElementById("fraction_encoding_todo")) {
            var checkedRadio = Array.from(document.querySelectorAll("[name=tdt]")).filter(x => x.checked)[0];
            enableRadioSubInputs(todoDueRadios, checkedRadio);
        } else {}
    }
    attachRadioSwitch(fractionEncodingRadios, enableActiveTodoDueType);
    enableRadioSubInputs(fractionEncodingRadios, document.getElementById("fraction_encoding_event"));

    const submitButton = document.getElementById("submit");
    createZipcodeCompleter(document.getElementById("zipcode_q"),
        document.getElementById("zipcode_results"),
        document.getElementById("zipcode"),
        document.getElementById("street_q"),
        submitButton
    );

    createStreetCompleter(document.getElementById("street_q"),
        document.getElementById("street_results"),
        document.getElementById("street"),
        document.getElementById("zipcode"),
        document.getElementById("house_number"),
        submitButton
    );

    document.getElementById("house_number").oninput = debounce(function() {
        submitButton.disabled = false;
        getFractions(
            document.getElementById("zipcode").value,
            document.getElementById("street").value,
            document.getElementById("house_number").value
        ).then(fractions => {
            let lc = getLangCode();
                names = fractions.map(fraction => fraction.name[lc]);
                nodes = fractions.map(fraction => {
                let input = document.createElement("input");
                input.type = "checkbox";
                input.name = "fif";
                input.id = `fraction_${fraction.id}`;
                input.value = fraction.id;
                input.checked = true;
                input.disabled = true;
                let label = document.createElement("label");
                label.textContent = fraction.name[lc];
                label.for = `fraction_${fraction.id}`;
                let container = document.createElement("div");
                container.replaceChildren(input, label);
                return container;
            }); 
            document.getElementById("fraction_container").replaceChildren(...nodes);
        });
    }, 250);
    document.getElementById("fractions").onchange = (e) => {
      e.target.labels[0].textContent = e.target.checked ? "All fractions" : "Selected fractions";
      Array.from(document.getElementById("fraction_container").getElementsByTagName("input"))
             .map((node) => {
                 node.disabled = e.target.checked;
             });
    };

    createEditableList(document.getElementById("reminder_list"), document.getElementById("reminder_prototype"), document.getElementById("add_reminder_button"));
};

main();
