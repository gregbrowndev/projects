import '../styles/globals.css';
import type { AppProps } from 'next/app';
import React from 'react';
import Head from 'next/head';

export default function App({ Component, pageProps }: AppProps) {
  return (
    <main
      role="main"
      className="mx-auto max-w-screen-lg overflow-hidden py-12 px-4 sm:px-6 lg:px-8"
    >
      <Head>
        <title>Jedi Demo</title>
        <link rel="icon" href="/static/temporal.ico" />
      </Head>
      <Component {...pageProps} />
    </main>
  );
}
