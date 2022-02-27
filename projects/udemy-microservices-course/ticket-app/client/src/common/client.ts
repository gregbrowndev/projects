import axios from "axios";
import { User } from "./models/user";
import { InvalidParam } from "./models/api-error";

export type PayloadErrors<T> = {
  [k in keyof T]?: string;
};

export type SignInPayload = {
  email: string;
  password: string;
};

export type SignInSuccess = {
  state: "success";
};

export type SignInError = {
  state: "error";
  title: string;
  detail?: string;
  errors: PayloadErrors<SignInPayload>;
};

export type SignInResult = SignInSuccess | SignInError;

export function signIn(payload: SignInPayload): Promise<SignInResult> {
  return axios
    .post<User>("/api/users/signin", payload)
    .then((res) => {
      console.log("[signin] response received: ", res);
      return { state: "success" } as SignInSuccess;
    })
    .catch((err) => {
      console.log("[signin] error caught: ", err);
      let submitError: SignInError = {
        state: "error",
        title: "Something went wrong",
        errors: {},
      };

      if (axios.isAxiosError(err)) {
        console.log("[signin] error response: ", err.response);
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
}

export type SignUpPayload = {
  email: string;
  password: string;
};

export type SignUpSuccess = {
  state: "success";
};

export type SignUpError = {
  state: "error";
  title: string;
  detail?: string;
  errors: PayloadErrors<SignUpPayload>;
};

export type SignUpResult = SignUpSuccess | SignUpError;

export function signUp(payload: SignUpPayload): Promise<SignUpResult> {
  return axios
    .post<User>("/api/users/signup", payload)
    .then((res) => {
      console.log("[signup] response received: ", res);
      return { state: "success" } as SignUpSuccess;
    })
    .catch((err) => {
      console.log("[signup] error caught: ", err);
      let submitError: SignUpError = {
        state: "error",
        title: "Something went wrong",
        errors: {},
      };

      if (axios.isAxiosError(err)) {
        console.log("[signin] error response: ", err.response);
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
}
