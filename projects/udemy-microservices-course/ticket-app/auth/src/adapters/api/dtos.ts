import * as t from 'io-ts';

export const InvalidParamsDTO = t.array(
  t.type({
    name: t.string,
    reason: t.string,
  }),
);
export type InvalidParamsDTO = t.TypeOf<typeof InvalidParamsDTO>;

export const ErrorResponseDTO = t.intersection([
  t.strict({
    title: t.string,
    statusCode: t.number,
  }),
  t.partial({
    detail: t.string,
    invalidParams: InvalidParamsDTO,
  }),
]);
export type ErrorResponseDTO = t.TypeOf<typeof ErrorResponseDTO>;

export const UserDTO = t.strict({
  id: t.string,
  email: t.string,
});
export type UserDTO = t.TypeOf<typeof UserDTO>;

export const CurrentUserDTO = t.intersection([
  t.strict({}),
  t.partial({
    currentUser: UserDTO,
  }),
]);
export type CurrentUserDTO = t.TypeOf<typeof CurrentUserDTO>;

export const SignInPayloadDTO = t.strict({
  email: t.string,
  password: t.string,
});
export type SignInPayloadDTO = t.TypeOf<typeof SignInPayloadDTO>;

export const SignInSuccessDTO = UserDTO;
export type SignInSuccessDTO = t.TypeOf<typeof SignInSuccessDTO>;

export const SignUpPayloadDTO = t.strict({
  email: t.string,
  password: t.string,
});
export type SignUpPayloadDTO = t.TypeOf<typeof SignUpPayloadDTO>;

export const SignUpSuccessDTO = t.strict({});
export type SignUpSuccessDTO = t.TypeOf<typeof SignUpSuccessDTO>;
