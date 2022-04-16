import Alert from "../../components/Alert";
import { ComponentMeta, ComponentStory } from "@storybook/react";

export default {
  title: "Components/Alert",
  component: Alert,
  args: {
    title: "Example Title",
    children:
      "The alert description provides context to the user, bringing attention to information that needs to be consumed.",
  },
} as ComponentMeta<typeof Alert>;

const Template: ComponentStory<typeof Alert> = (args) => <Alert {...args} />;

export const Success = Template.bind({});
Success.args = {
  variant: "success",
};

export const Warning = Template.bind({});
Warning.args = {
  variant: "warning",
};

export const Danger = Template.bind({});
Danger.args = {
  variant: "danger",
};

export const Info = Template.bind({});
Info.args = {
  variant: "info",
};

export const DangerWithChildren = Template.bind({});
DangerWithChildren.args = {
  variant: "danger",
  children: (
    <ul className="text-xs font-light text-gray-500">
      <li>Email must be supplied</li>
      <li>Password must have at least 6 characters</li>
    </ul>
  ),
};
