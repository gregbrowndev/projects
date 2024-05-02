# To Do App

## Questions

- Is the tsconfig.json inside the `server` project necessary given the codebase is bundled by the
  downstream `web-ui` project? What effect does this bundling have? Must the projects share the
  exact same TS configuration? Does the `server` code still get compiled and built as its own
  project? If not and given we may use Turborepo, will this lead to duplicate work if the `server`
  is consumed by multiple end-user apps?


## Aims

- Compact - minimise cost, cold-starts and cold-start time

  As a monolithic, full-stack app that can be deployed as a single FaaS, the possibility of a cold
  start is reduced because the same function can perform the UI rendering (SSR) and server-side logic.

  This also reduces the number of FaaS invocations since SSR data fetching and any access to the DB
  does not require additional network calls.

  However, bundling a larger application together may increase the bundle size and therefore hurt
  cold-start performance. 



## Overview

### How this project is structured

The project is structured using Yarn Workspaces.

- `web-ui` contains a NextJS project
- `server` contains a GraphQL API

The `server` project is installed into `web-ui` as a dependency. This means NextJS 
is able to self-host the GraphQL API a monolithic, full-stack application. 

During client-side rendering (CSR), the browser makes GraphQL requests to the host server. 
Since NextJS hosts the GraphQL server as a route, it can serve these requests within the same
process. During server-side rendering (SSR), the Apollo Client uses `SchemaLink` instead of
`HttpLink` to handle the query in-memory. This means no additional network calls are required.

All of this is beneficial for cost and latency optimisation. Since the monolithic server is 
deployed on GCP Cloud Functions, rendering the UI and performing server actions are performed 
on same function type. This reduces the chance of cold starts which would be more likely if
NextJS and the server were hosted as separate functions.


## Development

Install pnpm:

```
corepack enable pnpm
```

Install turbo:

```
pnpm install turbo --global
```


Install monorepo packages:

```
yarn install
```



Start `dev` in `web-ui` workspace:

```
yarn workspace web-ui dev
```



Viewing workspaces info:

```
yarn workspaces info   
```

Add dependency to root workspace:

```
yarn add --dev -W prettier
```

Creating a new workspace:

```
npm init -w ./packages/server
```

