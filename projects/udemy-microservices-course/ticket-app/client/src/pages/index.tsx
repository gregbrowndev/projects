import type { NextPage, NextPageContext } from "next";
import axios from "axios";
import { User } from "../common/models/user";

export interface HomeProps {
  currentUser?: User;
}

const Home: NextPage<HomeProps> = ({ currentUser }: HomeProps) => {
  return (
    <div>
      <h1>Welcome {currentUser?.email}</h1>
    </div>
  );
};

Home.getInitialProps = async (ctx: NextPageContext) => {
  try {
    console.log("I'm on the server");
    const response = await axios.get<User>("/api/users/currentuser");
    return { currentUser: response.data };
  } catch (err: Error | any) {
    console.log("Error when fetching current user");
    return {};
  }
};

export default Home;
