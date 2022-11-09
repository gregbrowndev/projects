import type { NextPage } from 'next';
import React, { useCallback, useEffect, useState } from 'react';
import {
  deleteWorkflow,
  getOrderStatus,
  getWorkflowIdCookie,
  startWorkflow,
} from '../services/DataService';
import Button from '../components/Button';
import Head from 'next/head';
import { OrderStatus } from '../temporal/lib/workflows';
import { useInterval } from '../hooks/useInterval';
import { isErrorData } from './api/utils';

interface StartBlockProps {
  onStart: () => Promise<void>;
}
const StartBlock: React.FC<StartBlockProps> = ({ onStart }) => {
  // const onStart = async (e: React.MouseEvent<HTMLButtonElement>) => {};
  return (
    <div>
      <h2 className="text-2xl">Workflow not started</h2>
      <div className="mt-3">
        <Button
          type="button"
          variant="primary"
          onClick={onStart}
          label="Start"
        />
      </div>
    </div>
  );
};

interface WorkflowBlockProps {
  workflowId: string;
  orderStatus: OrderStatus;
  setOrderStatus: (orderStatus: OrderStatus) => void;
  onStartAgain: () => {};
}
const WorkflowBlock: React.FC<WorkflowBlockProps> = ({
  workflowId,
  onStartAgain,
  orderStatus,
  setOrderStatus,
}) => {
  return (
    <div>
      <h2 className="text-2xl">Workflow started: {workflowId}</h2>
      <p>Status: {orderStatus}</p>
      <div className="mt-3">
        <Button
          type="button"
          variant="secondary"
          onClick={onStartAgain}
          label="Start Again"
        />
      </div>
    </div>
  );
};

const WaitingBlock = () => {
  // Waiting for order
};

const ExecutingBlock = () => {
  // Executing for order
};

const ResultBlock = () => {
  // View result of order
};

const Home: NextPage = () => {
  const [workflowId, setWorkflowId] = useState<string | undefined>(undefined);
  const [orderStatus, setOrderStatus] = useState<OrderStatus>('WAITING');
  const [resultConfirmed, setResultConfirmed] = useState<boolean>(true);

  useEffect(() => {
    const cookieVal = getWorkflowIdCookie();
    setWorkflowId(cookieVal);
  }, []);

  const fetchOrderStatus = useCallback(async () => {
    console.log('UseCallBack called');
    // const orderStatus = undefined;
    const orderStatus = await getOrderStatus().then((res) => {
      if (!isErrorData(res)) {
        return res.orderStatus;
      }
    });

    setOrderStatus(orderStatus || 'WAITING');
  }, [getOrderStatus]);

  // useInterval(
  //   () => {
  //     if (!workflowId) {
  //       return;
  //     }
  //     fetchOrderStatus().catch(console.error);
  //   },
  //   1000,
  //   [workflowId, fetchOrderStatus],
  // );

  const startWorkflowAndSaveID = async () => {
    return await startWorkflow().then(() => {
      const cookieVal = getWorkflowIdCookie();
      setWorkflowId(cookieVal);
    });
  };

  const deleteWorkflowAndRemoveId = async () => {
    return await deleteWorkflow().then(() => {
      const cookieVal = getWorkflowIdCookie();
      setWorkflowId(cookieVal);
    });
  };

  let block = StartBlock({ onStart: startWorkflowAndSaveID });
  if (workflowId) {
    block = WorkflowBlock({
      workflowId,
      orderStatus,
      setOrderStatus,
      onStartAgain: deleteWorkflowAndRemoveId,
    });
  }

  return (
    <>
      <Head>
        <title>Jedi Demo</title>
        <link rel="icon" href="/static/temporal.ico" />
      </Head>

      <section>
        <h1 className="text-6xl font-bold">
          <a className="text-blue-600" href="https://temporal.io">
            Temporal
          </a>{' '}
          Demo
        </h1>

        <p className="mt-3 text-2xl">
          Doing important Jedi work using orchestration
        </p>
      </section>

      <section className="mt-16">
        <div className="bg-white p-8 shadow-lg sm:rounded-md">
          {/* Inner content */}
          {block}
        </div>
      </section>
    </>
  );
};

export default Home;
