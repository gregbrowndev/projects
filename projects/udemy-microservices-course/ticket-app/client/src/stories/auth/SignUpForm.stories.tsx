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

export const WithErrors = Template.bind({});
WithErrors.args = {
  onSubmit: (values) =>
    Promise.resolve({
      title: "Invalid credentials",
      errors: {
        email: "Must be a valid email",
        password: "Must contain at least 6 characters",
      },
    }),
};
