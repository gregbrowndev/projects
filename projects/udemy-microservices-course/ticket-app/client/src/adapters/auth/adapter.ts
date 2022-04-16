import * as httpClient from "./http";
import { ErrorResponseDTO, SignInSuccessDTO, SignUpSuccessDTO } from "./http";
import {
  signInErrorDtoToCore,
  signInPayloadToDto,
  signInSuccessDtoToCore,
  signUpErrorDtoToCore,
  signUpPayloadToDto,
  signUpSuccessDtoToCore,
} from "./converters";
import {
  SignInError,
  SignInPayload,
  SignInResult,
  SignUpError,
  SignUpPayload,
  SignUpResult,
} from "../../types";

export function signIn(payload: SignInPayload): Promise<SignInResult> {
  const payloadDto = signInPayloadToDto(payload);
  return httpClient
    .signIn(payloadDto)
    .then((result: SignInSuccessDTO | ErrorResponseDTO) => {
      if (SignInSuccessDTO.is(result)) {
        return signInSuccessDtoToCore(result);
      } else {
        return signInErrorDtoToCore(result);
      }
    })
    .catch((error) => {
      const submitError: SignInError = {
        state: "error",
        title: "Something went wrong",
        errors: {},
      };
      return submitError;
    });
}

export function signUp(payload: SignUpPayload): Promise<SignUpResult> {
  const payloadDto = signUpPayloadToDto(payload);
  return httpClient
    .signUp(payloadDto)
    .then((result: SignUpSuccessDTO | ErrorResponseDTO) => {
      if (ErrorResponseDTO.is(result)) {
        return signUpErrorDtoToCore(result);
      } else {
        return signUpSuccessDtoToCore(result);
      }
    })
    .catch((error) => {
      const submitError: SignUpError = {
        state: "error",
        title: "Something went wrong",
        errors: {},
      };
      return submitError;
    });
}
