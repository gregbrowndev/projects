// Next.js API route support: https://nextjs.org/docs/api-routes/introduction
import type { NextApiRequest, NextApiResponse } from 'next';
import { teaDrunkQuery } from '../../temporal/src/workflows';
import createClient from '../../temporal/src/client';

type Data = {
  workflowId: string;
  teaDrunk: number;
};

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse<Data>,
) {
  const client = await createClient();

  // TODO - get workflowId from session
  const workflowId = 'business-meaningful-id';

  const workflow = client.getHandle(workflowId);
  const teaDrunk = await workflow.query(teaDrunkQuery);

  res.status(200).json({ workflowId: workflowId, teaDrunk: teaDrunk });
}
