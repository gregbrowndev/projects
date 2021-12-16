import SignUpForm, {
  SignUpFormValues,
  SignUpFormSubmitError,
} from "../../modules/auth/SignUpForm";
import { User } from "../../common/models/user";
import Router from "next/router";
import axios from "axios";
import { ApiError, InvalidParam } from "../../common/models/api-error";

const Signup = () => {
  const onSubmit = async (
    values: SignUpFormValues
  ): Promise<SignUpFormSubmitError | null> => {
    console.log("[signup] onSubmit called");
    return axios
      .post<User>("/api/users/signup", values)
      .then((res) => {
        console.log("[signup] response received: ", res);
        Router.push("/");
        return null;
      })
      .catch((err) => {
        console.log("[signup] error caught: ", err);
        let submitError: SignUpFormSubmitError = {
          title: "Something went wrong",
          errors: {},
        };

        if (axios.isAxiosError(err)) {
          console.log("[signup] error response: ", err.response);
          const invalidParams: InvalidParam[] =
            err.response?.data?.invalidParams || [];

          // set fieldError on response
          for (let invalidParam of invalidParams) {
            if (invalidParam.name == "email") {
              submitError.errors.email = invalidParam.reason;
            } else if (invalidParam.name == "password") {
              submitError.errors.password = invalidParam.reason;
            }
          }
        }

        return submitError;
      });
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
      <SignUpForm
        initialValues={{ email: "", password: "" }}
        onSubmit={onSubmit}
      />
    </div>
  );
};

export default Signup;
