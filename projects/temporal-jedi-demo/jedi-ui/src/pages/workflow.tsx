import type { NextPage } from 'next';
import { GetServerSideProps } from 'next';
import React, { useState } from 'react';
import Head from 'next/head';
import { isErrorData, Order, WorkflowReportData } from '../server/types';

import * as client from '../client/http/client';
import * as server from '../server/queries';
import { useRouter } from 'next/router';
import { WorkflowReportCard } from '../client/components/app/WorkflowReportCard';
import { getWorkflowId } from '../server/cookies';

const CURRENT_USER = 'Darth Sidious';

const WorkflowPage: NextPage<ServerProps> = ({ workflowReport }) => {
  const router = useRouter();
  const [submittingOrder, setSubmittingOrder] = useState<Order | undefined>(
    undefined,
  );

  const startAgainHandler = async () => {
    await client.deleteWorkflow();
    await router.push('/');
  };

  const sendOrderHandler = async (order: Order) => {
    setSubmittingOrder(order);

    const response = await client.sendOrder(order);
    if (isErrorData(response)) {
      // TODO - add toaster for error message
      alert('Failed to send order');
      console.error(response);
    }

    setSubmittingOrder(undefined);
    await router.push('/orderReport');
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
        {/* Inner content */}
        <WorkflowReportCard
          workflowReport={workflowReport}
          onStartAgain={startAgainHandler}
          onSendOrder={sendOrderHandler}
          submittingOrder={submittingOrder}
          currentUser={CURRENT_USER}
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

export default WorkflowPage;
