import type { NextPage } from 'next';
import React, { useEffect, useState } from 'react';
import {
  deleteWorkflow,
  getWorkflowIdCookie,
  startWorkflow,
} from '../services/DataService';
import Button from '../components/Button';
import Head from 'next/head';

interface StartBlockProps {
  onStart: () => Promise<void>;
}
const StartBlock = ({ onStart }: StartBlockProps) => {
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
  onStartAgain: () => {};
}
const WorkflowBlock = ({ workflowId, onStartAgain }: WorkflowBlockProps) => {
  return (
    <div>
      <h2 className="text-2xl">Workflow started: {workflowId}</h2>
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

  useEffect(() => {
    const cookieVal = getWorkflowIdCookie();
    setWorkflowId(cookieVal);
  }, []);

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
