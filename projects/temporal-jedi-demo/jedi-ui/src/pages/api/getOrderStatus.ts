// Next.js API route support: https://nextjs.org/docs/api-routes/introduction
import type { NextApiRequest, NextApiResponse } from 'next';
import { OrderStatus, orderStatusQuery } from '../../temporal/src/workflows';
import createClient from '../../temporal/src/client';

type Data = {
  workflowId: string;
  orderStatus: OrderStatus;
};

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse<Data>,
) {
  const client = await createClient();

  // TODO - get workflowId from session
  const workflowId = 'business-meaningful-id';

  const workflow = client.getHandle(workflowId);
  const orderStatus = await workflow.query(orderStatusQuery);

  res.status(200).json({ workflowId: workflowId, orderStatus: orderStatus });
}
