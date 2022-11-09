// Next.js API route support: https://nextjs.org/docs/api-routes/introduction
import type { NextApiRequest, NextApiResponse } from 'next';
import { jediBusiness } from '../../temporal/src/workflows';
import { TASK_QUEUE } from '../../temporal/src/worker';
import { ErrorData, getWorkflowId, setWorkflowId, createClient } from './utils';

type Data = {
  workflowId: string;
};

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse<Data | ErrorData>,
) {
  const client = await createClient();

  let workflowId = getWorkflowId({ req, res });
  if (workflowId) {
    res.status(400).json({ message: 'Workflow already started' });
    return;
  }

  workflowId = 'business-meaningful-id';

  // Save workflowID as cookie so UI can track workflow
  setWorkflowId(workflowId, { req, res });

  const handle = await client.start(jediBusiness, {
    workflowId: workflowId,
    taskQueue: TASK_QUEUE,
    args: [],
  });

  res.status(200).json({ workflowId: handle.workflowId });
}
