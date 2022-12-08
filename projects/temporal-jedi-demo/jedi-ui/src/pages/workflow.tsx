import type { NextPage } from 'next';
import { GetServerSideProps } from 'next';
import React, { useState } from 'react';
import Button from '../components/Button';
import Head from 'next/head';
import { isErrorData, Order, WorkflowReportData } from '../server/types';
import { getWorkflowId } from '../server/utils';

import * as client from '../client/httpClient';
import * as server from '../server/queries';
import { useRouter } from 'next/router';

const CURRENT_USER = 'Darth Sidious';

interface WorkflowBlockProps {
  workflowReport: WorkflowReportData;
  onStartAgain: () => {};
  onSendOrder: (order: Order) => {};
  submittingOrder: Order | undefined;
}
const WorkflowBlock: React.FC<WorkflowBlockProps> = ({
  workflowReport,
  onStartAgain,
  onSendOrder,
  submittingOrder,
}) => {
  return (
    <div>
      <div className="flex flex-row items-center justify-between">
        <h2 className="text-lg md:text-2xl">
          Workflow started: {workflowReport.workflowId}
        </h2>
        <Button
          type="button"
          variant={
            workflowReport.workflowStatus == 'DONE' ? 'primary' : 'tertiary'
          }
          onClick={onStartAgain}
          label="Start Again"
        />
      </div>
      <p>Troopers Danced: {workflowReport.troopersDanced}</p>
      <p>Jedi Eliminated: {workflowReport.jediEliminated}</p>
      <p>Jedi Remaining: {workflowReport.jediRemaining}</p>
      <div className="mt-3 flex flex-row justify-center gap-4">
        {/* TODO - add transition, hide the button not clicked and centre other */}
        <Button
          type="button"
          variant="secondary"
          size="large"
          onClick={() =>
            onSendOrder({ type: 'Order66', fromUser: CURRENT_USER })
          }
          label={submittingOrder?.type == 'Order66' ? 'Submitting' : 'Order 66'}
          disabled={submittingOrder != null}
          loading={submittingOrder?.type == 'Order66'}
        />
        <Button
          type="button"
          variant="secondary"
          size="large"
          onClick={() =>
            onSendOrder({ type: 'Order67', fromUser: CURRENT_USER })
          }
          label={submittingOrder?.type == 'Order67' ? 'Submitting' : 'Order 67'}
          disabled={submittingOrder != null}
          loading={submittingOrder?.type == 'Order67'}
        />
      </div>
    </div>
  );
};

interface Props {
  workflowId: string;
  workflowReport: WorkflowReportData;
}
export const getServerSideProps: GetServerSideProps<Props> = async ({
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

const WorkflowPage: NextPage<Props> = ({ workflowReport }) => {
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
        <div className="bg-white px-4 py-3 shadow-lg sm:rounded-md md:px-8 md:py-6">
          {/* Inner content */}
          <WorkflowBlock
            workflowReport={workflowReport}
            onStartAgain={startAgainHandler}
            onSendOrder={sendOrderHandler}
            submittingOrder={submittingOrder}
          />
        </div>
      </section>
    </>
  );
};

export default WorkflowPage;
