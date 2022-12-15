import type { NextApiRequest, NextApiResponse } from 'next';
import { ErrorData, SendOrderData } from '../../server/types';
import { Order } from '../../temporal/src/types';
import { getWorkflowId } from '../../server/cookies';
import { sendOrder } from '../../server/commands';

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse<SendOrderData | ErrorData>,
) {
  console.log(`API: /api/sendOrder received ${req.body}`);

  const workflowId = getWorkflowId({ req, res });
  if (!workflowId) {
    res
      .status(400)
      .json({ type: 'error', message: 'Workflow not started', detail: null });
    return;
  }

  const body = req.body;
  if (!body.type || !body.fromUser) {
    return res.status(400).json({
      type: 'error',
      message: 'type or fromUser not found',
      detail: null,
    });
  }

  const order: Order = {
    type: body.type,
    fromUser: body.fromUser,
  };

  sendOrder(workflowId, order);
  res.status(200).json({ workflowId: workflowId });
}
