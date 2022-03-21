import axios from "axios";
import { User } from "./models/user";
import { InvalidParam } from "./models/api-error";
import * as httpClient from "./http/client";

export type PayloadErrors<T> = {
  [k in keyof T]?: string;
};

export type SignInPayload = {
  email: string;
  password: string;
};

export type SignInSuccess = {
  state: "success";
  user: User;
};

export type SignInError = {
  state: "error";
  title: string;
  detail?: string;
  errors: PayloadErrors<SignInPayload>;
};

export type SignInResult = SignInSuccess | SignInError;

type ErrorResponse = {
  // RFC 7807 standard error handling (not full adherence)
  // https://datatracker.ietf.org/doc/html/rfc7807#section-3

  title: string;
  detail?: string;
  statusCode: number;
  invalidParams?: InvalidParam[];
};

export function signIn(payload: SignInPayload): Promise<SignInResult> {
  return axios
    .post<User>("/api/users/signin", payload)
    .then((res) => {
      console.debug("[signin] response received: ", res);
      // TODO - separate Auth's User DTO from pure User type
      const result: SignInSuccess = { state: "success", user: res.data };
      return result;
    })
    .catch((err) => {
      console.debug("[signin] error caught: ", err);
      let submitError: SignInError = {
        state: "error",
        title: "Something went wrong",
        errors: {},
      };

      // waiting for https://github.com/axios/axios/pull/3816 to merge
      // to make isAxiosError<T, D> type safe
      if (axios.isAxiosError(err) && err.response) {
        console.debug("[signin] error response: ", err.response);

        const response = err.response.data as ErrorResponse;
        submitError.title = response.title;

        const invalidParams: InvalidParam[] = response.invalidParams || [];

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
      console.debug("[signup] response received: ", res);
      return { state: "success" } as SignUpSuccess;
    })
    .catch((err) => {
      console.debug("[signup] error caught: ", err);
      let submitError: SignUpError = {
        state: "error",
        title: "Something went wrong",
        errors: {},
      };

      if (axios.isAxiosError(err)) {
        console.debug("[signin] error response: ", err.response);
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
