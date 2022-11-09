import type { NextPage } from 'next';
import Head from 'next/head';
import React, { useEffect, useState } from 'react';
import {
  deleteWorkflow,
  getWorkflowIdCookie,
  startWorkflow,
} from '../services/DataService';
import Button from '../components/Button';

interface StartBlockProps {
  onStart: () => Promise<void>;
}
const StartBlock = ({ onStart }: StartBlockProps) => {
  // const onStart = async (e: React.MouseEvent<HTMLButtonElement>) => {};
  return (
    <div>
      <Button type="button" variant="primary" onClick={onStart} label="Start" />
    </div>
  );
};

interface WorkflowBlockProps {
  workflowId: string;
  onStartAgain: () => {};
}
const WorkflowBlock = ({ workflowId, onStartAgain }: WorkflowBlockProps) => {
  return (
    <div>
      <p className="text-2xl">Workflow started: {workflowId}</p>
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
  // const workflowId = 'dummyID';
  const [workflowId, setWorkflowId] = useState<string | undefined>(undefined);

  useEffect(() => {
    const cookieVal = getWorkflowIdCookie();
    setWorkflowId(cookieVal);
  }, []);

  // TODO - try to get workflowId from cookie

  // TODO - pass onClick handler into StartBlock which calls /api/startWorkflow
  //  and extracts workflowId from cookie

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
      onStartAgain: deleteWorkflowAndRemoveId,
    });
  }

  return (
    <div className="flex min-h-screen flex-col items-center justify-center py-2">
      <Head>
        <title>Create Next App</title>
        <link rel="icon" href="/public/favicon.ico" />
      </Head>

      <main className="flex w-full flex-1 flex-col items-center justify-center px-20 text-center">
        <h1 className="text-6xl font-bold">
          <a className="text-blue-600" href="https://temporal.io">
            Temporal
          </a>{' '}
          Demo
        </h1>

        <p className="mt-3 text-2xl">
          Doing important Jedi work using Orchestration
        </p>

        <section className="mt-6 flex max-w-4xl justify-around sm:w-full">
          {block}
        </section>
      </main>
    </div>
  );
};

export default Home;
