const nextJest = require('next/jest');

const createJestConfig = nextJest({
  // Provide the path to your Next.js app to load next.config.js and .env files in your test environment
  dir: './',
});

// Add any custom config to be passed to Jest
/** @type {import('jest').Config} */
const customJestConfig = {
  // Add more setup options before each test is run
  setupFilesAfterEnv: ['<rootDir>/jest.setup.js'],
  // if using TypeScript with a baseUrl set to the root directory then you need the below for alias' to work
  moduleDirectories: ['node_modules', '<rootDir>/'],
  testEnvironment: 'jest-environment-jsdom',
};

// createJestConfig is exported this way to ensure that next/jest can load the Next.js config which is async
module.exports = async (...args) => {
  const fn = createJestConfig(customJestConfig)
  const res = await fn(...args)

  const esModules = ['@temporal/testing']
  const escapedEsModules = esModules.map(m => m.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')).join('|')

  // NextJS +12 has its own Rust compiler that is supposed to support Jest out of the box.
  // Additionally, the Rust compiler replaces Babel. However, if you import any dependencies
  // into your tests that have TypeScript keywords like `export`, you'll get an error.
  // This happens when MockActivityEnvironment is imported for testing the Temporal activities.
  // The solution is to make sure @temporal/testing is transformed by Jest. The code below means
  // ignore everything in node_modules apart from @temporal/testing, what isn't ignored is
  // transpiled into JS
  // See https://github.com/vercel/next.js/discussions/34774#discussioncomment-2246460
  res.transformIgnorePatterns = res.transformIgnorePatterns.map(pattern => {
    if (pattern === '/node_modules/') {
      return `/node_modules/(?!(${escapedEsModules}))/`
    }
    return pattern
  })

  return res
}