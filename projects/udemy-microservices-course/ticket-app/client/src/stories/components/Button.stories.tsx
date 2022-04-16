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
  variant: "primary",
  label: "Button",
};

export const Secondary = Template.bind({});
Secondary.args = {
  variant: "secondary",
  label: "Button",
};

export const Tertiary = Template.bind({});
Tertiary.args = {
  variant: "tertiary",
  label: "Button",
};

export const Large = Template.bind({});
Large.args = {
  variant: "primary",
  size: "large",
  label: "Button",
};

export const Small = Template.bind({});
Small.args = {
  variant: "primary",
  size: "small",
  label: "Button",
};
