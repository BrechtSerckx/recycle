const langCode = 'NL';

// Attach a custom submit function to the form.
function attachFormSubmit(form) {

    // Function to execute when the form is submitted.
    function submit() {
        // get form data
        const formData = new FormData(form);
        for (var entry of formData.entries()) {
            console.log(entry[0], entry[1]);
        }
    }

    form.addEventListener("submit", (event) => {
        event.preventDefault();
        submit();
    });
}

// Enable a date range type radio input and its corresponding date range inputs.
function enableDateRangeType(radios, thisRadio) {
    thisRadio.checked = true;
    // for all other radio inputs, disable the corresponding inputs
    for (thatRadio of radios) {
        if (thatRadio.id != thisRadio.id) {

            for (input of document.getElementsByClassName(thatRadio.id)) {
                input.disabled = true;
            }
        } else {}
    }
    // enable the corresponding inputs for this radio
    for (input of document.getElementsByClassName(thisRadio.id)) {
        input.disabled = false;
    }

}

// Attach a function to the `date_range_type` radio inputs so their respective
// input fields get enabled or disabled based on the radio input.
function attachDateRangeTypeFields(radios) {
    for (const radio of radios) {
        radio.onclick = () => {
            enableDateRangeType(radios, radio);
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
function main() {
    attachFormSubmit(document.getElementById("recycleForm"));

    const radios = document.getElementsByName("date_range_type");
    attachDateRangeTypeFields(radios);
    enableDateRangeType(radios, document.getElementById("date_range_type_rel"));

    const submitButton = document.getElementById("submit");
    createZipcodeCompleter(document.getElementById("zipcode_q"),
        document.getElementById("zipcode_results"),
        document.getElementById("zipcode"),
        document.getElementById("street_q"),
        submitButton
    );
};

main();
