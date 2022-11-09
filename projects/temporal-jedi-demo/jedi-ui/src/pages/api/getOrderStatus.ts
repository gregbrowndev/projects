// Next.js API route support: https://nextjs.org/docs/api-routes/introduction
import type { NextApiRequest, NextApiResponse } from 'next';
import { OrderStatus, orderStatusQuery } from '../../temporal/src/workflows';
import { ErrorData, getWorkflowId, createClient } from './utils';

type Data = {
  workflowId: string;
  orderStatus: OrderStatus;
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
  const orderStatus = await workflow.query(orderStatusQuery);

  res.status(200).json({ workflowId: workflowId, orderStatus: orderStatus });
}
