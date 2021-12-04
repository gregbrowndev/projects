import InputGroup from "../../components/InputGroup";
import { ComponentMeta, ComponentStory } from "@storybook/react";

export default {
  title: "Components/InputGroup",
  component: InputGroup,
} as ComponentMeta<typeof InputGroup>;

const Template: ComponentStory<typeof InputGroup> = (args) => (
  <InputGroup {...args} />
);

export const TextInput = Template.bind({});
TextInput.args = {
  inputType: "text",
  label: "Label",
  description: "Optional description",
  required: false,
};

export const EmailInput = Template.bind({});
EmailInput.args = {
  inputType: "email",
  label: "Label",
  description: "Optional description",
  required: false,
};

export const PasswordInput = Template.bind({});
PasswordInput.args = {
  inputType: "email",
  label: "Label",
  description: "Optional description",
  required: false,
};

export const InputRequired = Template.bind({});
InputRequired.args = {
  inputType: "text",
  label: "Label",
  description: "Optional description",
  required: true,
};
