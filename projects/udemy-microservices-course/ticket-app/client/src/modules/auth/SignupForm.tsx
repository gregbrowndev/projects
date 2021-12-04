import Button from "../../components/Button";
import InputGroup from "../../components/InputGroup";

interface SignupFormProps {}

const SignupForm = ({ ...props }: SignupFormProps) => {
  return (
    <form>
      <div className="shadow sm:rounded-md sm:overflow-hidden">
        <div className="px-4 py-5 bg-white space-y-6 sm:p-6">
          <InputGroup
            inputType="email"
            label="Email Address"
            required
            placeholder="you@example.com"
          />
          <InputGroup inputType="password" label="Password" required />
        </div>
        <div className="px-4 py-3 bg-gray-50 text-right sm:px-6">
          <Button label="Sign Up" />
        </div>
      </div>
    </form>
  );
};

export default SignupForm;
