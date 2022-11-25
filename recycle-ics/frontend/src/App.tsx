import { useForm, SubmitHandler } from "react-hook-form";

enum LangCode {
  NL = "nl",
  FR = "fr",
  DE = "de",
  EN = "en",
}

type Inputs = {
  example: string;
  exampleRequired: string;
  lc: LangCode;
};

export default function App() {
  const {
    register,
    handleSubmit,
    watch,
    formState: { errors },
  } = useForm<Inputs>();
  const onSubmit: SubmitHandler<Inputs> = (data) => console.log(data);

  console.log(watch("example")); // watch input value by passing the name of it

  return (
    <>
      <h1>Recycle ICS generator</h1>
      <p>Generate ICS files and links for your waste collections. </p>
      <form onSubmit={handleSubmit(onSubmit)}>
        <h2>Generator form</h2>
        <h3>Language</h3>
        <p>
          Choose in which language the waste collection titles and descriptions
          should be.
        </p>
        <fieldset>
          <legend>Language</legend>
          <label>
            <input
              type="radio"
              value={LangCode.NL}
              {...register("lc")}
              defaultChecked
            />
            Nederlands
          </label>
          <label>
            <input type="radio" value={LangCode.FR} {...register("lc")} />
            Francais
          </label>
          <label>
            <input type="radio" value={LangCode.DE} {...register("lc")} />
            Deutsch
          </label>
          <label>
            <input type="radio" value={LangCode.EN} {...register("lc")} />
            English
          </label>
        </fieldset>
        <input defaultValue="test" {...register("example")} />

        {/* include validation with required or other standard HTML validation rules */}
        <input {...register("exampleRequired", { required: true })} />
        {/* errors will return when field validation fails  */}
        {errors.exampleRequired && <span>This field is required</span>}

        <input type="submit" />
      </form>
    </>
  );
}
