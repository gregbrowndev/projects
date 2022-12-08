import type { NextPage } from 'next';
import { GetServerSideProps } from 'next';
import React, { useState } from 'react';
import Button from '../components/Button';
import Head from 'next/head';
import { getWorkflowId } from '../server/utils';

import * as client from '../client/httpClient';
import { useRouter } from 'next/router';
import { isErrorData } from '../server/types';

interface Props {}
export const getServerSideProps: GetServerSideProps<Props> = async ({
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

const HomePage: NextPage<Props> = (props) => {
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
        <div className="bg-white px-4 py-3 shadow-lg sm:rounded-md md:px-8 md:py-6">
          {/* Inner content */}
          <div>
            <h2 className="text-2xl">Workflow not started</h2>
            <div className="mt-3 flex flex-row justify-center gap-4">
              <Button
                type="button"
                size="large"
                variant="primary"
                onClick={startHandler}
                disabled={starting}
                loading={starting}
                label={starting ? 'Starting' : 'Start'}
              />
            </div>
          </div>
        </div>
      </section>
    </>
  );
};

export default HomePage;
