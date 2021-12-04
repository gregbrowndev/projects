import "../styles/globals.css";
import { addDecorator } from "@storybook/react";
import * as NextImage from "next/image";
import { withConsole } from "@storybook/addon-console";

const OriginalNextImage = NextImage.default;

Object.defineProperty(NextImage, "default", {
  configurable: true,
  value: (props) => <OriginalNextImage {...props} unoptimized />,
});

export const parameters = {
  actions: { argTypesRegex: "^on[A-Z].*" },
  showPanel: true,
  panelPosition: "bottom",
  controls: {
    matchers: {
      color: /(background|color)$/i,
      date: /Date$/,
    },
  },
};

addDecorator((storyFn, context) => withConsole()(storyFn)(context));
