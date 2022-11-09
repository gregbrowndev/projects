// Next.js API route support: https://nextjs.org/docs/api-routes/introduction
import type { NextApiRequest, NextApiResponse } from 'next';
import { Order, orderSignal } from '../../temporal/src/workflows';
import { ErrorData, getWorkflowId, createClient } from './utils';

type Data = {
  workflowId: string;
};

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse<Data | ErrorData>,
) {
  const workflowId = getWorkflowId({ req, res });
  if (!workflowId) {
    res.status(400).json({ message: 'Workflow not started' });
    return;
  }

  const body = req.body;

  if (!body.type || !body.fromUser) {
    return res.status(400).json({ message: 'type or fromUser not found' });
  }

  const order: Order = {
    type: body.type,
    fromUser: body.fromUser,
  };

  console.log(`API: /api/sendOrder received ${req.body}`);

  const client = await createClient();

  const workflow = client.getHandle(workflowId);
  await workflow.signal(orderSignal, order);

  res.status(200).json({ workflowId: workflowId });
}
