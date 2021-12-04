module.exports = {
  stories: [
    "../src/stories/**/*.stories.mdx",
    "../src/stories/**/*.stories.@(js|jsx|ts|tsx)",
  ],
  addons: [
    "@storybook/addon-links",
    "@storybook/addon-essentials",
    "@storybook/addon-a11y",
    "@storybook/addon-storysource",
    // Tailwind v2 requires PostCSS v8. However, StoryBook bundles v7.
    // see https://github.com/wagerfield/storybook-tailwind
    {
      name: "@storybook/addon-postcss",
      options: {
        postcssLoaderOptions: {
          implementation: require("postcss"),
        },
      },
    },
  ],
  staticDirs: ["../public"],
  framework: "@storybook/react",
  core: {
    builder: "webpack5",
  },
};
