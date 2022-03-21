import * as t from "io-ts";

export const ErrorResponseDTO = t.intersection([
  t.type({
    title: t.string,
    statusCode: t.number,
  }),
  t.partial({
    detail: t.string,
    invalidParams: t.array(
      t.type({
        name: t.string,
        reason: t.string,
      })
    ),
  }),
]);
export type ErrorResponseDTO = t.TypeOf<typeof ErrorResponseDTO>;

export const UserDTO = t.type({
  id: t.string,
  email: t.string,
});
export type UserDTO = t.TypeOf<typeof UserDTO>;

export const SignInPayloadDTO = t.type({
  email: t.string,
  password: t.string,
});
export type SignInPayloadDTO = t.TypeOf<typeof SignInPayloadDTO>;

export const SignInSuccessDTO = UserDTO;
export type SignInSuccessDTO = t.TypeOf<typeof SignInSuccessDTO>;

export const SignUpPayloadDTO = t.type({
  email: t.string,
  password: t.string,
});
export type SignUpPayloadDTO = t.TypeOf<typeof SignUpPayloadDTO>;

export const SignUpSuccessDTO = t.type({});
export type SignUpSuccessDTO = t.TypeOf<typeof SignUpSuccessDTO>;
