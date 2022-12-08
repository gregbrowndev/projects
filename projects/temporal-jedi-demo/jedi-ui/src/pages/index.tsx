import type { NextPage } from 'next';
import { GetServerSideProps } from 'next';
import React, { useMemo, useState } from 'react';
import Head from 'next/head';

import * as client from '../client/http/client';
import { useRouter } from 'next/router';
import { isErrorData } from '../server/types';
import { StartCard } from '../client/components/app/StartCard';
import { getWorkflowId } from '../server/cookies';
import { START_GIFS } from '../client/gifs';
import { randomInt } from '../client/utils';

const HomePage: NextPage<ServerProps> = (props) => {
  const router = useRouter();
  const [starting, setStarting] = useState<boolean>(false);

  const startHandler = async () => {
    console.log('startHandler clicked');
    setStarting(true);
    const response = await client.startWorkflow();
    if (isErrorData(response)) {
      // TODO - add toaster for error message
      alert('Failed to start');
      console.error(response);
    } else {
      console.log('success, redirecting...');
      await router.push('/workflow');
    }
    setStarting(false);
    // TODO - add fun starting GIF?
    // setTimeout(() => setStarting(false), 5000);
  };

  const gifs = useMemo(() => START_GIFS, []);
  const selectedGif = useMemo(() => {
    // TODO - it would be better to cycle through them to avoid showing the same one. Need to store some state globally?
    const index = randomInt(0, gifs.length - 1);
    return gifs[index];
  }, [gifs]);
  // TODO - add button to refresh GIF

  return (
    <>
      <Head>
        <title>Jedi Demo</title>
        <link rel="icon" href="/static/temporal.ico" />
      </Head>

      <section>
        <h1 className="text-2xl font-bold md:text-6xl">
          <a className="text-blue-600" href="https://temporal.io">
            Temporal
          </a>{' '}
          Demo
        </h1>

        <p className="mt-1 text-lg md:mt-3 md:text-2xl">
          Doing important Jedi work using orchestration
        </p>
      </section>

      <section className="mt-6 md:mt-16">
        {/* Inner content */}
        <StartCard
          onStartHandler={startHandler}
          starting={starting}
          image={selectedGif}
        />
      </section>
    </>
  );
};

interface ServerProps {}

export const getServerSideProps: GetServerSideProps<ServerProps> = async ({
  req,
  res,
}) => {
  const workflowId = getWorkflowId({ req, res });
  if (workflowId) {
    return {
      redirect: {
        destination: '/workflow',
        permanent: false,
      },
    };
  }
  return {
    props: {},
  };
};

export default HomePage;
