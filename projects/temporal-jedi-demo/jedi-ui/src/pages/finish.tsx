import { GetServerSideProps, NextPage } from 'next';
import { getWorkflowId } from '../server/cookies';
import Head from 'next/head';
import React, { useMemo, useState } from 'react';
import { FinishCard } from '../client/components/app/FinishCard';
import { WorkflowReportData } from '../server/types';
import * as server from '../server/queries';
import * as client from '../client/http/client';
import { useRouter } from 'next/router';
import { FINISH_GIFS } from '../client/gifs';
import { randomInt } from '../client/utils';

const FinishPage: NextPage<ServerProps> = ({ workflowReport }) => {
  const router = useRouter();
  const [startingAgain, setStartingAgain] = useState<boolean>(false);
  const startAgainHandler = async () => {
    setStartingAgain(true);
    await client.deleteWorkflow();
    await router.push('/');
    setStartingAgain(false);
  };

  const gifs = useMemo(() => FINISH_GIFS, []);
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
        <h1 className="text-2xl font-bold md:text-6xl">The End</h1>

        <p className="mt-1 text-lg md:mt-3 md:text-2xl">
          Thanks for listening!
        </p>
      </section>

      <section className="mt-6 md:mt-16">
        {/* Inner content */}
        <FinishCard
          workflowReport={workflowReport}
          onStartAgain={startAgainHandler}
          startingAgain={startingAgain}
          image={selectedGif}
        />
      </section>
    </>
  );
};

interface ServerProps {
  workflowId: string;
  workflowReport: WorkflowReportData;
}

export const getServerSideProps: GetServerSideProps<ServerProps> = async ({
  req,
  res,
}) => {
  const workflowId = getWorkflowId({ req, res });
  if (!workflowId) {
    return {
      redirect: {
        destination: '/',
        permanent: false,
      },
    };
  }
  const workflowReport = await server.getWorkflowReport(workflowId);
  if (workflowReport?.currentOrderStatus == 'EXECUTING') {
    return {
      redirect: {
        destination: '/orderReport',
        permanent: false,
      },
    };
  }

  return {
    props: {
      workflowId,
      workflowReport,
    },
  };
};

export default FinishPage;
