import axios from "axios";
import {
  ErrorResponseDTO,
  SignInPayloadDTO,
  SignInSuccessDTO,
  SignUpPayloadDTO,
  SignUpSuccessDTO,
} from "./dtos";
import { fold, left } from "fp-ts/Either";
import { PathReporter } from "io-ts/PathReporter";
import { pipe } from "fp-ts/function";

export class DecodeError extends Error {
  name = "DecodeError";
}

export async function signIn(
  payload: SignInPayloadDTO
): Promise<SignInSuccessDTO | ErrorResponseDTO> {
  const data = SignInPayloadDTO.encode(payload);
  return await axios
    .post(
      "/api/users/signin",
      data
      // { validateStatus: false }
    )
    .then((res) => {
      console.debug("[signin] response received: ", res);
      return pipe(
        SignInSuccessDTO.decode(res.data),
        fold(
          (error) => {
            // TODO - shouldn't resort to throw exceptions when using FP
            throw new DecodeError(PathReporter.report(left(error)).join("\n"));
          },
          (data) => data
        )
      );
    })
    .catch((error: unknown) => {
      console.debug("[signin] error caught: ", error);

      if (!axios.isAxiosError(error)) {
        // We didn't expect this and we don't know how to handle it, rethrow
        throw error;
      }

      if (error.response != null) {
        return pipe(
          ErrorResponseDTO.decode(error.response.data),
          fold(
            (error) => {
              // TODO - shouldn't resort to throw exceptions when using FP
              throw new DecodeError(
                PathReporter.report(left(error)).join("\n")
              );
            },
            (data) => data
          )
        );
      }

      throw new Error("Sign in request failed for unknown reason");
    });
}

export function signUp(
  payload: SignUpPayloadDTO
): Promise<SignUpSuccessDTO | ErrorResponseDTO> {
  const data = SignUpPayloadDTO.encode(payload);
  return axios
    .post("/api/users/signup", data)
    .then((res) => {
      console.debug("[signup] response received: ", res);
      return pipe(
        SignUpSuccessDTO.decode(res.data),
        fold(
          (error) => {
            // TODO - shouldn't resort to throw exceptions when using FP
            throw new DecodeError(PathReporter.report(left(error)).join("\n"));
          },
          (data) => data
        )
      );
    })
    .catch((error) => {
      console.debug("[signup] error caught: ", error);

      if (!axios.isAxiosError(error)) {
        // We didn't expect this and we don't know how to handle it, rethrow
        throw error;
      }

      if (error.response != null) {
        return pipe(
          ErrorResponseDTO.decode(error.response.data),
          fold(
            (error) => {
              // TODO - shouldn't resort to throw exceptions when using FP
              throw new DecodeError(
                PathReporter.report(left(error)).join("\n")
              );
            },
            (data) => data
          )
        );
      }

      throw new Error("Sign up request failed for unknown reason");
    });
}
