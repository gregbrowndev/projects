# To Do App

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


```
cd packages/web-ui
yarn install
```

```
yarn workspace web-ui dev
```

Viewing workspaces info:

```
yarn workspaces info   
```

Creating a new package:

```
npm init -w ./packages/server
```