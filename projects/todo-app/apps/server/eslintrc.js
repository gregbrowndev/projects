/** @type {import("eslint").Linter.Config} */
module.exports = {
    extends: ["@repo/eslint-config/server.js"],
    jsparser: "@typescript-eslint/parser",
    parserOptions: {
        project: true,
    },
};