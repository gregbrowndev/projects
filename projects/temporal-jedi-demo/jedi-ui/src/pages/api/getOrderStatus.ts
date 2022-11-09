import type { NextApiRequest, NextApiResponse } from 'next';
import { OrderStatus, orderStatusQuery } from '../../temporal/src/workflows';
import { ErrorData, getWorkflowId, createClient } from './utils';

export type GetOrderStatusData = {
  workflowId: string;
  orderStatus: OrderStatus;
};

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse<GetOrderStatusData | ErrorData>,
) {
  const client = await createClient();

  const workflowId = getWorkflowId({ req, res });
  if (!workflowId) {
    res.status(400).json({ type: 'error', message: 'Workflow not started' });
    return;
  }

  const workflow = client.getHandle(workflowId);
  const orderStatus = await workflow.query(orderStatusQuery);

  res.status(200).json({ workflowId: workflowId, orderStatus: orderStatus });
}
