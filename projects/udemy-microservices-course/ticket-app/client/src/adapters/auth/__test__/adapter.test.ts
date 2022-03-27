import * as adapter from "../index";
import axios from "axios";
import MockAdapter from "axios-mock-adapter";
import {
  SignInError,
  SignInSuccess,
  SignUpError,
  SignUpSuccess,
} from "../../../types";
import { ErrorResponseDTO, SignInSuccessDTO, SignUpSuccessDTO } from "../http";

describe("adapters/auth", () => {
  let mock: MockAdapter;

  beforeAll(() => {
    mock = new MockAdapter(axios);
  });

  afterEach(() => {
    mock.reset();
  });

  describe("signIn", () => {
    describe("when correct credentials are provided", () => {
      it("should return SignInSuccess", async () => {
        // given
        const resp: SignInSuccessDTO = { id: "1", email: "test@test.com" };
        mock.onPost(`/api/users/signin`).reply(200, resp);

        // when
        let result = await adapter.signIn({
          email: "test@test.com",
          password: "password123",
        });

        // then
        const expected: SignInSuccess = { state: "success", user: resp };
        expect(result).toEqual(expected);
      });
    });

    describe("when incorrect credentials are provided", () => {
      it("should return SignInError", async () => {
        // given
        const resp: ErrorResponseDTO = {
          title: "Invalid credentials",
          statusCode: 400,
          invalidParams: [],
        };
        mock.onPost(`/api/users/signin`).reply(400, resp);

        // when
        let result = await adapter.signIn({
          email: "test@test.com",
          password: "password123",
        });

        // then
        const expected: SignInError = {
          title: "Invalid credentials",
          state: "error",
          errors: {},
        };
        expect(result).toEqual(expected);
      });
    });
  });

  describe("signUp", () => {
    it("should return SignUpSuccess when correct credentials are provided", async () => {
      // given
      const resp: SignUpSuccessDTO = {};
      mock.onPost(`/api/users/signup`).reply(200, resp);

      // when
      let result = await adapter.signUp({
        email: "test@test.com",
        password: "password123",
      });

      // then
      const expected: SignUpSuccess = { state: "success" };
      expect(result).toEqual(expected);
    });

    it("should return SignInError when incorrect credentials are provided", async () => {
      // given
      const resp: ErrorResponseDTO = {
        title: "Invalid credentials",
        statusCode: 400,
        invalidParams: [],
      };
      mock.onPost(`/api/users/signup`).reply(400, resp);

      // when
      let result = await adapter.signUp({
        email: "test@test.com",
        password: "password123",
      });

      // then
      const expected: SignUpError = {
        title: "Invalid credentials",
        state: "error",
        errors: {},
      };
      expect(result).toEqual(expected);
    });
  });
});
