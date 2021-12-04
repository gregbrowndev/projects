import Button from "../../components/Button";
import InputGroup from "../../components/InputGroup";
import { FormEvent, useState } from "react";

interface SignupFormProps {}

const SignupForm = ({}: SignupFormProps) => {
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");

  const onSubmit = (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault();

    console.log(email, password);
  };

  return (
    <form onSubmit={onSubmit}>
      <div className="shadow sm:rounded-md sm:overflow-hidden">
        <div className="px-4 py-5 bg-white space-y-6 sm:p-6">
          <InputGroup
            inputType="email"
            value={email}
            label="Email Address"
            required
            placeholder="you@example.com"
            onChange={(e) => setEmail(e.target.value)}
          />
          <InputGroup
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

export default SignupForm;
