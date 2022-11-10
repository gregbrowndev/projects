import { Html, Head, Main, NextScript } from 'next/document';
import React from 'react';

export default function Document() {
  return (
    // eslint-disable-next-line react/jsx-no-undef
    <Html className="h-full w-full bg-gray-50 font-sans text-gray-600 antialiased">
      <Head />
      <body className="h-full w-full">
        <Main />
        <NextScript />
      </body>
    </Html>
  );
}
