import AuthForm, {
  AuthFormValues,
  AuthFormSubmitError,
} from "../../modules/auth/AuthForm";
import Router from "next/router";
import { signUp } from "../../adapters/auth/adapter";

const SignUp = () => {
  const onSubmit = async (
    values: AuthFormValues
  ): Promise<AuthFormSubmitError | null> => {
    console.log("[signup] onSubmit called");
    return signUp({ email: values.email, password: values.password }).then(
      (result) => {
        console.log("[signup] response received: ", result);
        switch (result.state) {
          case "success":
            Router.push("/");
            return null;
          case "error":
            return result;
        }
      }
    );
  };

  return (
    <div>
      <div className="flex flex-col mb-10">
        <h1 className="text-5xl leading-none font-bold text-gray-900 tracking-tight mb-4">
          Sign Up
        </h1>
        <p className="text-2xl tracking-tight text-gray-500">
          Sign up to Stubhub to experience the best movies
        </p>
      </div>
      <AuthForm
        type="sign-up"
        initialValues={{ email: "", password: "" }}
        onSubmit={onSubmit}
      />
    </div>
  );
};

export default SignUp;
