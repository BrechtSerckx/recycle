const langCode = 'NL';

// Attach a custom submit function to the form.
function attachFormSubmit(form, permalink, downloadLink) {

    // Function to execute when the form is submitted.
    function submit() {
        // get form data
        const formData = new FormData(form);
        for (var entry of formData.entries()) {
            console.log(entry[0], entry[1]);
        }

        // prepare formdata
        formData.set("lang_code", langCode);

        // make url
        var url = new URL('/api/generate', window.location.origin);
        url.search = new URLSearchParams(formData).toString();

        // set permalink
        permalink.value = url;

        // set download link
        downloadLink.disabled = false;
        downloadLink.href = url;

        // show permalink and download link
        document.getElementById("link").style = "hidden";
    }

    form.addEventListener("submit", (event) => {
        event.preventDefault();
        submit();
    });
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

function removeChildren(node) {
    while (node.firstChild) {
        node.removeChild(node.lastChild);
    }
}

function createAutoCompleter(input, container, getData, mkLabel, withResult, threshold = 3) {
    input.oninput = function() {
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
    };
};

function createZipcodeCompleter(input, container, target, streetQInput, submitButton) {
    createAutoCompleter(input,
        container,
        q => searchZipcodes(q),
        res => res.city.name,
        res => {
            target.value = res.id;
            streetQInput.disabled = false;
            submitButton.disabled = true;
        }
    );
}

function createStreetCompleter(input, container, target, zipcodeInput, houseNumberInput, submitButton) {
    createAutoCompleter(input,
        container,
        q => searchStreets(zipcodeInput.value, q),
        res => res.name,
        res => {
            target.value = res.id;
            houseNumberInput.disabled = false;
            submitButton.disabled = false;
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
        document.getElementById("downloadLink")
    );

    const dateRangeRadios = document.getElementsByName("date_range_type");
    attachRadioSwitch(dateRangeRadios);
    enableRadioSubInputs(dateRangeRadios, document.getElementById("date_range_type_rel"));

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

    createEditableList(document.getElementById("reminder_list"), document.getElementById("reminder_prototype"), document.getElementById("add_reminder_button"));
};

main();
