import SignUpForm from "../../modules/auth/SignUpForm";

const Signup = () => {
  return (
    <div>
      <div className="flex flex-col mb-10">
        <h1 className="text-5xl leading-none font-bold text-gray-900 tracking-tight mb-4">
          Sign Up
        </h1>
        <p className="text-2xl tracking-tight text-gray-500">
          Sign up to Stubhub to experience the best movies
        </p>
      </div>
      <SignUpForm />
    </div>
  );
};

export default Signup;
