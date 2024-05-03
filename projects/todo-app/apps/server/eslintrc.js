/** @type {import("eslint").Linter.Config} */
module.exports = {
    extends: ["@repo/config-eslint/server.js"],
    jsparser: "@typescript-eslint/parser",
    parserOptions: {
        project: true,
    },
};