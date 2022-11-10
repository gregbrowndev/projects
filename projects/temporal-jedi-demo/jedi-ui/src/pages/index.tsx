import type { NextPage } from 'next';
import React, { useCallback, useEffect, useState } from 'react';
import {
  deleteWorkflow,
  getOrderStatus,
  getTeaDrunk,
  getWorkflowIdCookie,
  sendOrder,
  startWorkflow,
} from '../services/DataService';
import Button from '../components/Button';
import Head from 'next/head';
import { useInterval } from '../hooks/useInterval';
import { isErrorData, Order, OrderStatus } from '../services/hack';

const REFRESH_INTERVAL_MS = 500;
const CURRENT_USER = 'Darth Sidious';

interface StartBlockProps {
  onStart: () => Promise<void>;
}
const StartBlock: React.FC<StartBlockProps> = ({ onStart }) => {
  // const onStart = async (e: React.MouseEvent<HTMLButtonElement>) => {};
  return (
    <div>
      <h2 className="text-2xl">Workflow not started</h2>
      <div className="mt-3 flex flex-row justify-center gap-4">
        <Button
          type="button"
          size="large"
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
  teaDrunk: number;
  onStartAgain: () => {};
  onSendOrder: (order: Order) => {};
}
const WorkflowBlock: React.FC<WorkflowBlockProps> = ({
  workflowId,
  orderStatus,
  teaDrunk,
  onStartAgain,
  onSendOrder,
}) => {
  return (
    <div>
      <div className="flex flex-row items-center justify-between">
        <h2 className="text-2xl">Workflow started: {workflowId}</h2>
        <Button
          type="button"
          variant={orderStatus == 'DONE' ? 'primary' : 'tertiary'}
          onClick={onStartAgain}
          label="Start Again"
        />
      </div>
      <p>Status: {orderStatus}</p>
      <p>Tea drunk: {teaDrunk}</p>
      <div className="mt-3 flex flex-row justify-center gap-4">
        <Button
          type="button"
          variant="secondary"
          onClick={() =>
            onSendOrder({ type: 'Order66', fromUser: CURRENT_USER })
          }
          label="Order 66"
          disabled={orderStatus != 'WAITING'}
        />
        <Button
          type="button"
          variant="secondary"
          onClick={() =>
            onSendOrder({ type: 'Order67', fromUser: CURRENT_USER })
          }
          label="Order 67"
          disabled={orderStatus != 'WAITING'}
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
  const [teaDrunk, setTeaDrunk] = useState<number>(0);
  const [resultConfirmed, setResultConfirmed] = useState<boolean>(true);

  useEffect(() => {
    const cookieVal = getWorkflowIdCookie();
    setWorkflowId(cookieVal);
  }, []);

  const fetchOrderStatus = useCallback(async () => {
    console.log('fetchOrderStatus called');
    const orderStatus = await getOrderStatus().then((res) => {
      if (!isErrorData(res)) {
        return res.orderStatus;
      }
    });

    setOrderStatus(orderStatus || 'WAITING');
  }, [getOrderStatus]);

  const fetchTeaDrunk = useCallback(async () => {
    console.log('fetchTeaDrunk called');
    const teaDrunk = await getTeaDrunk().then((res) => {
      if (!isErrorData(res)) {
        return res.teaDrunk;
      }
    });

    if (teaDrunk) {
      setTeaDrunk(teaDrunk);
    }
  }, [getTeaDrunk]);

  useInterval(
    () => {
      if (!workflowId) {
        return;
      }
      fetchOrderStatus().catch(console.error);
      fetchTeaDrunk().catch(console.error);
    },
    REFRESH_INTERVAL_MS,
    [REFRESH_INTERVAL_MS, workflowId, fetchOrderStatus, fetchTeaDrunk],
  );

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
      setOrderStatus('WAITING');
      setTeaDrunk(0);
    });
  };

  const sendOrderAndSetStatus = async (order: Order) => {
    setOrderStatus('EXECUTING');
    await sendOrder(order);
  };
  let block = StartBlock({ onStart: startWorkflowAndSaveID });
  if (workflowId) {
    block = WorkflowBlock({
      workflowId,
      orderStatus,
      teaDrunk,
      onStartAgain: deleteWorkflowAndRemoveId,
      onSendOrder: sendOrderAndSetStatus,
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
        <div className="bg-white px-8 py-6 shadow-lg sm:rounded-md">
          {/* Inner content */}
          {block}
        </div>
      </section>
    </>
  );
};

export default Home;
