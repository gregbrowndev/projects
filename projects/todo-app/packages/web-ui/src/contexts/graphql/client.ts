import {cacheExchange, Client, createClient, fetchExchange, SSRExchange, ssrExchange} from '@urql/next';
import {executeExchange} from '@urql/exchange-execute';
import {schema} from "@repo/server/src/schema";


export const makeSSRExchange = () => {
    const isClient = typeof window !== 'undefined';
    return ssrExchange({
        isClient
    });
}

export const makeGraphQLClient: (ssr: SSRExchange) => Promise<Client> = async (ssr) => {
    const isClient = typeof window !== 'undefined';

    let transportExchange = fetchExchange;

    if (!isClient) {
        // TODO: does this need to be dynamically imported to avoid server code leaking into client-side bundle?
        // const { schema } = await import("@repo/server/src/schema")
        transportExchange = executeExchange({
            schema
        })
    }

    return createClient({
        url: 'http://localhost:3000/api/graphql',
        exchanges: [cacheExchange, ssr, transportExchange],
        suspense: true,
    });
}