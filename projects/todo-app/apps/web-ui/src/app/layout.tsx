import type { Metadata } from "next";
import { Inter } from "next/font/google";
import "./globals.css";
import GraphqlProvider from "@/contexts/graphql/provider";

const inter = Inter({ subsets: ["latin"] });

export const metadata: Metadata = {
  title: "To Do App",
  description: "A To-Do app demonstrating a compact full-stack monolith architecture",
};

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
      <html lang="en">
        <body className={inter.className}>
            <GraphqlProvider>
                {children}
            </GraphqlProvider>
        </body>
      </html>
  );
}
