import Button from "../../components/Button";
import InputGroup from "../../components/InputGroup";
import { FormEvent, useState } from "react";
import useRequest from "../../hooks/use-request";
import Router from "next/router";
import { User } from "../../common/models/user";

export interface SignUpFormProps {}

const SignUpForm = ({}: SignUpFormProps) => {
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");
  const { doRequest, errors } = useRequest<User>({
    url: "/api/users/signup",
    method: "post",
    data: {
      email,
      password,
    },
    onSuccess: (obj: User) => Router.push("/"),
  });

  const onSubmit = async (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    await doRequest();
  };

  return (
    <form onSubmit={onSubmit}>
      <div className="shadow-lg sm:rounded-md sm:overflow-hidden">
        <div className="px-4 py-5 bg-white space-y-6 sm:p-6">
          {errors}
          <InputGroup
            id="email-control"
            inputType="email"
            value={email}
            label="Email Address"
            required
            placeholder="you@example.com"
            onChange={(e) => setEmail(e.target.value)}
          />
          <InputGroup
            id="password-control"
            inputType="password"
            value={password}
            label="Password"
            required
            onChange={(e) => setPassword(e.target.value)}
          />
        </div>
        <div className="px-4 py-3 bg-gray-50 text-right sm:px-6">
          <Button label="Sign Up" />
        </div>
      </div>
    </form>
  );
};

export default SignUpForm;
