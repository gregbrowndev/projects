import { User } from "./adapters/auth/models/user";

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
