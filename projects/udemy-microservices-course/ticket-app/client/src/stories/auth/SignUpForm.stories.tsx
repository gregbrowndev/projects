import SignUpForm from "../../modules/auth/SignUpForm";
import { ComponentStory } from "@storybook/react";

export default {
  title: "Auth/Sign Up Form",
  component: SignUpForm,
};

const Template: ComponentStory<typeof SignUpForm> = (args) => (
  <SignUpForm {...args} />
);

export const Empty = Template.bind({});
Empty.args = {};

// TODO - add story showing form with errors
