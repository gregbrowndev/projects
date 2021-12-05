import "../../styles/globals.css";
import type { AppProps } from "next/app";

function MyApp({ Component, pageProps }: AppProps) {
  return (
    <div className="overflow-hidden">
      <main
        role="main"
        className="max-w-screen-lg mx-auto py-12 px-4 sm:px-6 lg:px-8"
      >
        <Component {...pageProps} />
      </main>
    </div>
  );
}

export default MyApp;
