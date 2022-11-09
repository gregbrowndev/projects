// Next.js API route support: https://nextjs.org/docs/api-routes/introduction
import type { NextApiRequest, NextApiResponse } from 'next';
import { teaDrunkQuery } from '../../temporal/src/workflows';
import { getWorkflowId, ErrorData, createClient } from './utils';

type Data = {
  workflowId: string;
  teaDrunk: number;
};

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse<Data | ErrorData>,
) {
  const client = await createClient();

  const workflowId = getWorkflowId({ req, res });
  if (!workflowId) {
    res.status(400).json({ message: 'Workflow not started' });
    return;
  }

  const workflow = client.getHandle(workflowId);
  const teaDrunk = await workflow.query(teaDrunkQuery);

  res.status(200).json({ workflowId: workflowId, teaDrunk: teaDrunk });
}
