"use client";

import React, { useMemo } from "react";
import { UrqlProvider } from "@urql/next";
import { makeGraphQLClient } from "./client";

export default function GraphqlProvider({ children }: React.PropsWithChildren) {
  const [client, ssr] = useMemo(makeGraphQLClient, []);
  return (
    <UrqlProvider client={client} ssr={ssr}>
      {children}
    </UrqlProvider>
  );
}
