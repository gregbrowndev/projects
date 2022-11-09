// Next.js API route support: https://nextjs.org/docs/api-routes/introduction
import type { NextApiRequest, NextApiResponse } from 'next';
import {
  Order,
  orderSignal,
  teaDrunkQuery,
} from '../../temporal/src/workflows';
import createClient from '../../temporal/src/client';

type Data = {
  workflowId: string;
};

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse<Data | any>,
) {
  const body = req.body;

  if (!body.type || !body.fromUser) {
    return res.status(400).json({ data: 'type or fromUser not found' });
  }

  const order: Order = {
    type: body.type,
    fromUser: body.fromUser,
  };

  console.log(`API: /api/sendOrder received ${req.body}`);

  const client = await createClient();

  // TODO - get workflowId from session
  const workflowId = 'business-meaningful-id';

  const workflow = client.getHandle(workflowId);
  await workflow.signal(orderSignal, order);

  res.status(200).json({ workflowId: workflowId });
}
