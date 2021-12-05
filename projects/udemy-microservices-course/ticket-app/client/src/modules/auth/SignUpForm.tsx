import Button from "../../components/Button";
import InputGroup from "../../components/InputGroup";
import { FormEvent, useState } from "react";
import axios, { AxiosError } from "axios";
import Alert from "../../components/Alert";

interface SignUpFormProps {}

interface Error {
  message: string;
}
interface ErrorList {
  errors: Error[];
}

const SignUpForm = ({}: SignUpFormProps) => {
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");
  const [errors, setErrors] = useState<Error[]>([]);

  const onSubmit = async (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    try {
      const response = await axios.post("/api/users/signup", {
        email,
        password,
      });
      console.log(response.data);
    } catch (err) {
      if (err.response?.data?.errors.length > 0) {
        setErrors(err.response.data.errors);
      } else {
        setErrors([{ message: "Something went wrong" }]);
      }
    }
  };

  return (
    <form onSubmit={onSubmit}>
      <div className="shadow-lg sm:rounded-md sm:overflow-hidden">
        <div className="px-4 py-5 bg-white space-y-6 sm:p-6">
          {errors.length > 0 && (
            <div className="p-2">
              <Alert variant="danger" title="Oops...">
                <ul className="text-xs font-light text-gray-500">
                  {errors.map((error) => (
                    <li key={error.message}>{error.message}</li>
                  ))}
                </ul>
              </Alert>
            </div>
          )}
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
