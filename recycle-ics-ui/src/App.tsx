import { useForm, FormProvider } from "react-hook-form";
import * as React from "react";
import { LanguageSection } from "./section/language";
import AddressSection from "./section/address";
import FilterSection from "./section/filter";
import DateRangeSection from "./section/daterange";
import EncodingSection from "./section/encoding";
import DownloadSection from "./section/download";
import DescriptionSection from "./section/description";
import { FormInputs, defaultFormInputs } from "./types";
import { githubUrl } from "./env";

function GithubCorner() {
    return (
        <a href={githubUrl} target="_blank" rel="noreferrer">
            <img
                style={{
                    position: "fixed",
                    top: 0,
                    right: 0,
                    border: 0,
                    zIndex: 9999,
                }}
                src="https://github.blog/wp-content/uploads/2008/12/forkme_right_darkblue_121621.png"
                alt="Fork me on GitHub"
            />
        </a>
    );
}

export function App() {
    const formContext = useForm<FormInputs>({
        defaultValues: defaultFormInputs,
    });

    return (
        <>
            <GithubCorner />
            <h1>Recycle ICS generator</h1>
            <DescriptionSection />
            <FormProvider {...formContext}>
                <form>
                    <h2>Generator form</h2>
                    <LanguageSection />
                    <AddressSection />
                    <FilterSection />
                    <DateRangeSection />
                    <EncodingSection />
                    <h2>Generate ICS</h2>
                    <DownloadSection />
                </form>
            </FormProvider>
        </>
    );
}
