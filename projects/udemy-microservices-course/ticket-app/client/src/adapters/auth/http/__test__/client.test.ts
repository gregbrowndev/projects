import axios from "axios";
import MockAdapter from "axios-mock-adapter";

import { ErrorResponseDTO, SignInSuccessDTO, UserDTO } from "../dtos";
import { signIn } from "../client";

describe("auth/http/client", () => {
  let mock: MockAdapter;

  beforeAll(() => {
    mock = new MockAdapter(axios);
  });

  afterEach(() => {
    mock.reset();
  });

  describe("signIn", () => {
    describe("when correct credentials are provided", () => {
      it("should return SignInSuccessDTO", async () => {
        // given
        const resp: UserDTO = { id: "1", email: "test@test.com" };
        mock.onPost(`/api/users/signin`).reply(200, resp);

        // when
        let result = await signIn({
          email: "test@test.com",
          password: "password123",
        });

        // then
        const expected: SignInSuccessDTO = resp;
        expect(result).toEqual(expected);
      });
    });

    describe("when incorrect credentials are provided", () => {
      it("should return SignInErrorDTO", async () => {
        // given
        const resp: ErrorResponseDTO = {
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
        expect(result).toEqual(resp);
      });
    });
  });
});
