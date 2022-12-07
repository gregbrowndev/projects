import type { NextApiRequest, NextApiResponse } from 'next';
import { Order, orderSignal } from '../../temporal/src/workflows';
import { getWorkflowId, createWorkflowClient } from '../../server/utils';
import { ErrorData } from '../../server/types';

export type SendOrderData = {
  workflowId: string;
};

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse<SendOrderData | ErrorData>,
) {
  const workflowId = getWorkflowId({ req, res });
  if (!workflowId) {
    res.status(400).json({ type: 'error', message: 'Workflow not started' });
    return;
  }

  const body = req.body;

  if (!body.type || !body.fromUser) {
    return res
      .status(400)
      .json({ type: 'error', message: 'type or fromUser not found' });
  }

  const order: Order = {
    type: body.type,
    fromUser: body.fromUser,
  };

  console.log(`API: /api/sendOrder received ${req.body}`);

  const client = await createWorkflowClient();

  const workflow = client.getHandle(workflowId);
  await workflow.signal(orderSignal, order);

  res.status(200).json({ workflowId: workflowId });
}
