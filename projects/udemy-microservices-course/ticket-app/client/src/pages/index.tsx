import type { GetServerSideProps, NextPage } from "next";
import { User } from "../common/models/user";
import buildClient from "../api/build-client";

interface CurrentUserResponse {
  currentUser?: User;
}

export interface HomeProps {
  currentUser?: User;
}

const Home: NextPage<HomeProps> = ({ currentUser }: HomeProps) => {
  console.log("Current user: ", currentUser);
  return (
    <div>
      <h1>Welcome {currentUser?.email}</h1>
    </div>
  );
};

export const getServerSideProps: GetServerSideProps<HomeProps> = async (
  context
) => {
  const client = buildClient(context.req);

  try {
    const { data } = await client.get<CurrentUserResponse>(
      "api/users/currentuser"
    );
    return { props: { currentUser: data?.currentUser } };
  } catch (err: Error | any) {
    console.log("Error when fetching current user");
    console.error(err);
    return { props: {} };
  }
};

export default Home;
