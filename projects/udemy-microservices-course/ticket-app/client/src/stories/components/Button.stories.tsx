import Button from "../../components/Button";
import { ComponentMeta, ComponentStory } from "@storybook/react";
import React from "react";

export default {
  title: "Components/Button",
  component: Button,
} as ComponentMeta<typeof Button>;

const Template: ComponentStory<typeof Button> = (args) => <Button {...args} />;

export const Primary = Template.bind({});
Primary.args = {
  type: "primary",
  label: "Button",
};

export const Secondary = Template.bind({});
Secondary.args = {
  type: "secondary",
  label: "Button",
};

export const White = Template.bind({});
White.args = {
  type: "white",
  label: "Button",
};

export const Large = Template.bind({});
Large.args = {
  type: "primary",
  size: "large",
  label: "Button",
};

export const Small = Template.bind({});
Small.args = {
  type: "primary",
  size: "small",
  label: "Button",
};
