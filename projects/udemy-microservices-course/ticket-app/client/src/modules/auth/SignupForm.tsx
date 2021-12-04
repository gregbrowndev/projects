const SignupForm = () => {
  return (
    <form>
      <div>
        <label
          htmlFor="email"
          className="block text-sm font-medium text-gray-700"
        >
          Email Address
        </label>
        <input
          type="email"
          className="focus:ring-indigo-500 focus:border-indigo-500 block w-full pl-7 pr-12 sm:text-sm border-gray-300 rounded-md"
          id="email"
        />
      </div>
      <div className="">
        <label htmlFor="password">Password</label>
        <input type="password" className="" id="password" />
      </div>
      <button className="">Sign Up</button>
    </form>
  );
};

export default SignupForm;
