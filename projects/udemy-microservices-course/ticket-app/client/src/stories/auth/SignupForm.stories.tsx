import SignupForm from "../../modules/auth/SignupForm";
import { ComponentStory } from "@storybook/react";

export default {
  title: "Auth/Sign Up Form",
  component: SignupForm,
};

const Template: ComponentStory<typeof SignupForm> = (args) => (
  <SignupForm {...args} />
);

export const Empty = Template.bind({});
Empty.args = {};

// TODO - add story showing form with errors
