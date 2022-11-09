import { Html, Head, Main, NextScript } from 'next/document';
import React from 'react';

export default function Document() {
  return (
    // eslint-disable-next-line react/jsx-no-undef
    <Html className="h-full bg-gray-50">
      <Head />
      <body className="flex min-h-full flex-col font-sans text-gray-600 antialiased">
        <Main />
        <NextScript />
      </body>
    </Html>
  );
}
