const SignupForm = () => {
  return (
    <form>
      <div className="form-group">
        <label htmlFor="email">Email Address</label>
        <input type="email" className="form-control" id="email" />
      </div>
      <div className="form-group">
        <label htmlFor="password">Password</label>
        <input type="password" className="form-control" id="passwod" />
      </div>
      <button className="btn btn-primary">Sign Up</button>
    </form>
  );
};

export default SignupForm;
