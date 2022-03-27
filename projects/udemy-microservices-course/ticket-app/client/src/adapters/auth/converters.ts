import {
  ErrorResponseDTO,
  InvalidParamsDTO,
  SignInPayloadDTO,
  SignInSuccessDTO,
  SignUpPayloadDTO,
  SignUpSuccessDTO,
} from "./http";
import {
  PayloadErrors,
  SignInError,
  SignInPayload,
  SignInSuccess,
  SignUpError,
  SignUpPayload,
  SignUpSuccess,
} from "../../types";

export function signInPayloadToDto(payload: SignInPayload): SignInPayloadDTO {
  return {
    email: payload.email,
    password: payload.password,
  };
}

export function signInSuccessDtoToCore(dto: SignInSuccessDTO): SignInSuccess {
  return {
    state: "success",
    user: {
      id: dto.id,
      email: dto.email,
    },
  };
}

export function signInErrorDtoToCore(dto: ErrorResponseDTO): SignInError {
  // map invalidParams into payload errors
  const errors: PayloadErrors<SignInPayload> = {};
  for (let invalidParam of dto.invalidParams || []) {
    if (invalidParam.name == "email") {
      errors.email = invalidParam.reason;
    } else if (invalidParam.name == "password") {
      errors.password = invalidParam.reason;
    }
  }

  return {
    state: "error",
    title: dto.title,
    detail: dto.detail,
    errors,
  };
}

export function signUpPayloadToDto(payload: SignUpPayload): SignUpPayloadDTO {
  return {
    email: payload.email,
    password: payload.password,
  };
}

export function signUpSuccessDtoToCore(dto: SignUpSuccessDTO): SignUpSuccess {
  return {
    state: "success",
  };
}

export function signUpErrorDtoToCore(dto: ErrorResponseDTO): SignUpError {
  // map invalidParams into payload errors
  const errors: PayloadErrors<SignUpPayload> = {};
  for (let invalidParam of dto.invalidParams || []) {
    if (invalidParam.name == "email") {
      errors.email = invalidParam.reason;
    } else if (invalidParam.name == "password") {
      errors.password = invalidParam.reason;
    }
  }

  return {
    state: "error",
    title: dto.title,
    detail: dto.detail,
    errors,
  };
}
