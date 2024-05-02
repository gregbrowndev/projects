'use client';

import React, {useEffect, useMemo, useState} from 'react';
import {Client, UrqlProvider, SSRExchange} from '@urql/next';
import {makeGraphQLClient, makeSSRExchange} from "./client";

export default function GraphqlProvider({children}: React.PropsWithChildren) {
    // const [client, ssr] =  useMemo(makeGraphQLClient, []);

    const ssr = useMemo(makeSSRExchange, [])

    const [client, setClient] = useState<Client>();

    useEffect(() => {
        makeGraphQLClient(ssr)
            .then(_client => {
                    setClient(_client);
                }
            )
    }, [setClient, ssr])

    if (client == null || ssr == null) {
        console.log("No client found.");
        return (
            <div>Loading...</div>
        )
    }

    console.log("client found")

    return (
        <>
            <UrqlProvider client={client} ssr={ssr}>
                {children}
            </UrqlProvider>
        </>
    );
}