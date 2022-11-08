// Next.js API route support: https://nextjs.org/docs/api-routes/introduction
import type { NextApiRequest, NextApiResponse } from 'next';
import { jediBusiness } from '../../temporal/src/workflows';
import { TASK_QUEUE } from '../../temporal/src/worker';
import createClient from '../../temporal/src/client';

type Data = {
  workflowId: string;
};

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse<Data>,
) {
  // TODO - inject WorkflowClient?
  const client = await createClient();

  // TODO - add workflowId to the session
  const workflowId = 'business-meaningful-id';

  const handle = await client.start(jediBusiness, {
    workflowId: workflowId,
    taskQueue: TASK_QUEUE,
    args: [],
  });

  res.status(200).json({ workflowId: handle.workflowId });
}
