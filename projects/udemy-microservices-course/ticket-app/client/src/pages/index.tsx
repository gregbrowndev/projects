import type { NextPage, NextPageContext } from "next";

export interface HomeProps {
  colour: string;
}

const Home: NextPage<HomeProps> = ({ colour }: HomeProps) => {
  return (
    <div>
      <h1 className={`text-${colour}-600`}>Welcome</h1>
    </div>
  );
};

Home.getInitialProps = (ctx: NextPageContext) => {
  console.log("I am on the server!");
  return { colour: "red" };
};

export default Home;
