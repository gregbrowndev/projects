import { signIn, SignInError, SignInSuccess } from "../adapter";
import axios from "axios";
import MockAdapter from "axios-mock-adapter";
import { ErrorResponse } from "../../../../../auth/src/middlewares/error-handling";
import { User } from "../models/user";

describe("signIn", () => {
  let mock: MockAdapter;

  beforeAll(() => {
    mock = new MockAdapter(axios);
  });

  afterEach(() => {
    mock.reset();
  });

  describe("when correct credentials are provided", () => {
    it("should return SignInSuccess", async () => {
      // given
      const resp: User = { id: "1", email: "test@test.com" };
      mock.onPost(`/api/users/signin`).reply(200, resp);

      // when
      let result = await signIn({
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
      const resp: ErrorResponse = {
        title: "Invalid credentials",
        statusCode: 400,
        invalidParams: [],
      };
      mock.onPost(`/api/users/signin`).reply(400, resp);

      // when
      let result = await signIn({
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
