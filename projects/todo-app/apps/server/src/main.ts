import { server } from "./server";

// Note: the server is embedded into NextJS as a route. Executing this file can
// be used for debugging the server independent of NextJS.
server.listen(4000, () => {
  console.info("Server is running on http://localhost:4000/graphql");
});
