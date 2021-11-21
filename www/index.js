
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


function main() {
    attachFormSubmit(document.getElementById("recycleForm"));
};

main();
