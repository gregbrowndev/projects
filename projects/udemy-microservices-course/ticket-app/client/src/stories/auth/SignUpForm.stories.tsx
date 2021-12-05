import SignUpForm from "../../modules/auth/SignUpForm";
import { ComponentStory } from "@storybook/react";
import { rest } from "msw";

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
WithErrors.parameters = {
  msw: [
    rest.post("/api/users/signup", (_req, res, ctx) => {
      return res(
        ctx.status(400),
        ctx.json({
          errors: [{ message: "Invalid credentials" }],
        })
      );
    }),
  ],
};
WithErrors.args = {};
