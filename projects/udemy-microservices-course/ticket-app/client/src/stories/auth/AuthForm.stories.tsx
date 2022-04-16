import AuthForm from "../../modules/auth/AuthForm";
import { ComponentStory } from "@storybook/react";

export default {
  title: "Auth/Auth Form",
  component: AuthForm,
};

const Template: ComponentStory<typeof AuthForm> = (args) => (
  <AuthForm {...args} />
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
